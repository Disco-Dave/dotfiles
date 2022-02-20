#!/bin/bash

set -ex

# Update the system clock
timedatectl set-ntp true

# Prompt for passwords
echo -n "Enter device encryption password: "
read -r encryption_password

echo -n "Enter root password: "
read -r root_password

echo -n "Enter password for $USER: "
read -r user_password

# Securely wipe disks
echo -n "Type YES to securely wipe disks: "
read -r wipe_answer

if [[ "$wipe_answer" == "YES" ]]; then
  yes YES | cryptsetup open --type plain -d /dev/urandom /dev/nvme0n1
  dd bs=1M if=/dev/zero of=/dev/mapper/to_be_wiped status=progress || true
  cryptsetup close to_be_wiped
fi

# Partition the disks
parted --script /dev/nvme0n1 mklabel gpt
parted --script /dev/nvme0n1 mkpart boot fat32 1MiB 513MiB
parted --script /dev/nvme0n1 set 1 esp on
parted --script /dev/nvme0n1 mkpart luks 513MiB 100%

# Setup luks/lvm
yes "$encryption_password" | cryptsetup luksFormat --type luks /dev/nvme0n1p2
yes "$encryption_password" | cryptsetup --perf-no_read_workqueue --perf-no_write_workqueue --persistent open /dev/nvme0n1p2 cryptlvm
pvcreate /dev/mapper/cryptlvm
vgcreate main /dev/mapper/cryptlvm
lvcreate -L 32G main -n swap
lvcreate -l 100%FREE main -n root

# Format the partitions
mkfs.fat -F32 -n BOOT /dev/nvme0n1p1
mkswap -L SWAP /dev/main/swap
mkfs.ext4 -L ROOT /dev/main/root

# Mount the file systems
mount /dev/main/root /mnt
mkdir -p /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot
swapon /dev/main/swap

# Install essential packages
pacstrap /mnt base base-devel linux linux-firmware neovim networkmanager lvm2 intel-ucode

# Generate fstab
genfstab -L /mnt >> /mnt/etc/fstab

# Configure time
ln -sf /mnt/usr/share/zoneinfo/US/Eastern /mnt/etc/localtime
arch-chroot /mnt timedatectl set-timezone US/Eastern
arch-chroot /mnt timedatectl set-ntp true
arch-chroot /mnt hwclock --systohc

# Set localization
sed -i '/#en_US.UTF-8 UTF-8/s/^#//g' /mnt/etc/locale.gen
arch-chroot /mnt locale-gen
echo "LANG=en_US.UTF-8" > /mnt/etc/locale.conf

# Configure network
HOSTNAME="laptop"
hostnamectl hostname "$HOSTNAME"
echo "$HOSTNAME" > /mnt/etc/hostname
{
  echo "127.0.0.1     localhost"
  echo "::1           localhost"
  echo "127.0.0.1     $HOSTNAME"
} >> /mnt/etc/hosts
arch-chroot /mnt systemctl enable NetworkManager

# Generate initramfs
sed -i 's/^HOOKS=.*/HOOKS=\(base udev autodetect modconf block keyboard keymap encrypt lvm2 filesystems fsck\)/' /mnt/etc/mkinitcpio.conf
arch-chroot /mnt mkinitcpio -P

# Prompt to set root password
yes "$root_password" | arch-chroot /mnt passwd

# Configure boot loader
arch-chroot /mnt bootctl install
{
  echo "title     Arch Linux"
  echo "linux     /vmlinuz-linux"
  echo "initrd    /intel-ucode.img"
  echo "initrd    /initramfs-linux.img"
  echo "options   cryptdevice=UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/nvme0n1p2):cryptlvm:allow-discards  root=/dev/main/root rw"
} >> /mnt/boot/loader/entries/arch.conf

# Enable periodic trim for sdds
arch-chroot /mnt systemctl enable fstrim.timer

# Create main user
USER="david"
arch-chroot /mnt useradd --create-home --groups wheel --shell /bin/bash $USER
arch-chroot /mnt pacman -S --noconfirm --needed sudo
sed -i '/%wheel ALL=(ALL:ALL) ALL/s/^# //g' /mnt/etc/sudoers
yes "$user_password" | arch-chroot /mnt passwd $USER

# Enable automatic login
mkdir -p /mnt/etc/systemd/system/getty@tty1.service.d
{
  echo "[Service]"
  echo "ExecStart="
  echo "ExecStart=-/usr/bin/agetty --autologin $USER --noclear %I \$TERM"
} >> /mnt/etc/systemd/system/getty@tty1.service.d/override.conf

# Install required graphics driver
arch-chroot /mnt pacman -S --noconfirm --needed xf86-video-intel

# Copy the dotfiles folder into the new install
SCRIPT_PATH=$(realpath "$0")
DOTFILES_PATH=$(dirname "$SCRIPT_PATH" | xargs dirname)
mkdir -p "/mnt/home/$USER/.config"
arch-chroot /mnt chown -R $USER:$USER "/home/$USER/.config"
rsync -arv "$DOTFILES_PATH" "/mnt/home/$USER/.config/"
arch-chroot /mnt chown -R $USER:$USER "/home/$USER/.config/dotfiles"
