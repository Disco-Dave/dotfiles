#!/bin/bash

# Hard disks
# /dev/sda 2T HDD
# /dev/sdb 3T HDD
# /dev/sdc 500G SSD
# /dev/nvme0n1 500G SSD

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

echo -n "Enter password for ssh key: "
read -r ssh_password

# Securely wipe disks
echo -n "Type YES to securely wipe disks: "
read -r wipe_answer

if [[ "$wipe_answer" == "YES" ]]; then
  yes YES | cryptsetup open --type plain -d /dev/urandom /dev/sda to_be_wiped 
  dd bs=1M if=/dev/zero of=/dev/mapper/to_be_wiped status=progress || true
  cryptsetup close to_be_wiped

  yes YES | cryptsetup open --type plain -d /dev/urandom /dev/sdd to_be_wiped 
  dd bs=1M if=/dev/zero of=/dev/mapper/to_be_wiped status=progress || true
  cryptsetup close to_be_wiped

  yes YES | cryptsetup open --type plain -d /dev/urandom /dev/sdc to_be_wiped 
  dd bs=1M if=/dev/zero of=/dev/mapper/to_be_wiped status=progress || true
  cryptsetup close to_be_wiped

  yes YES | cryptsetup open --type plain -d /dev/urandom /dev/nvme0n1 to_be_wiped 
  dd bs=1M if=/dev/zero of=/dev/mapper/to_be_wiped status=progress || true
  cryptsetup close to_be_wiped
fi

# Partition the disks
parted --script /dev/sda mklabel gpt
parted --script /dev/sda mkpart luks 1MiB 100%

parted --script /dev/sdb mklabel gpt
parted --script /dev/sdb mkpart luks 1MiB 100%

parted --script /dev/sdc mklabel gpt
parted --script /dev/sdc mkpart luks 1MiB 100%

parted --script /dev/nvme0n1 mklabel gpt
parted --script /dev/nvme0n1 mkpart boot fat32 1MiB 513MiB
parted --script /dev/nvme0n1 set 1 esp on
parted --script /dev/nvme0n1 mkpart luks 513MiB 100%


# Setup luks/lvm
yes "$encryption_password" | cryptsetup luksFormat --type luks /dev/nvme0n1p2
yes "$encryption_password" | cryptsetup open /dev/nvme0n1p2 main
pvcreate /dev/mapper/main
vgcreate main /dev/mapper/main
lvcreate -L 64G main -n swap
lvcreate -l 100%FREE main -n root

yes "$encryption_password" | cryptsetup luksFormat --type luks /dev/sda1
yes "$encryption_password" | cryptsetup open /dev/sda1 docs

yes "$encryption_password" | cryptsetup luksFormat --type luks /dev/sdb1
yes "$encryption_password" | cryptsetup open /dev/sdb1 media

yes "$encryption_password" | cryptsetup luksFormat --type luks /dev/sdc1
yes "$encryption_password" | cryptsetup open /dev/sdc1 home

# Format the partitions
mkfs.fat -F32 -n BOOT /dev/nvme0n1p1
mkfs.ext4 -L ROOT /dev/main/root
mkswap -L SWAP /dev/main/swap
mkfs.ext4 -L DOCS /dev/mapper/docs
mkfs.ext4 -L MEDIA /dev/mapper/media
mkfs.ext4 -L HOME /dev/mapper/home

# Mount the file systems
mount /dev/main/root /mnt
mkdir -p /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot
mkdir -p /mnt/mnt/docs
mount /dev/mapper/docs /mnt/mnt/docs
mkdir -p /mnt/mnt/media
mount /dev/mapper/media /mnt/mnt/media
mkdir -p /mnt/home
mount /dev/mapper/home /mnt/home

# Install essential packages
pacstrap /mnt base base-devel linux linux-firmware neovim networkmanager lvm2 amd-ucode nvidia nvidia-settings nvidia-utils sudo openssh

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
HOSTNAME="desktop"
hostnamectl hostname "$HOSTNAME"
echo "$HOSTNAME" > /mnt/etc/hostname
{
  echo "127.0.0.1     localhost"
  echo "::1           localhost"
  echo "127.0.0.1     $HOSTNAME"
} >> /mnt/etc/hosts
arch-chroot /mnt systemctl enable NetworkManager

# Generate initramfs
{
  echo "main    UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/nvme0n1p2)  none  discard,no-read-workqueue,no-write-workqueue"
  echo "docs    UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/sda1)       none"
  echo "media   UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/sdb1)       none"
  echo "home    UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/sdc1)       none  discard,no-read-workqueue,no-write-workqueue"
} >> /mnt/etc/crypttab.initramfs

sed -i 's/^HOOKS=.*/HOOKS=\(base systemd autodetect keyboard modconf block sd-encrypt lvm2 filesystems fsck\)/' /mnt/etc/mkinitcpio.conf
arch-chroot /mnt mkinitcpio -P

# Enable periodic trim for sdds
arch-chroot /mnt systemctl enable fstrim.timer

# Prompt to set root password
yes "$root_password" | arch-chroot /mnt passwd

# Configure boot loader
arch-chroot /mnt bootctl install
{
  echo "title     Arch Linux"
  echo "linux     /vmlinuz-linux"
  echo "initrd    /amd-ucode.img"
  echo "initrd    /initramfs-linux.img"
  echo "options   root=/dev/main/root rw"
} >> /mnt/boot/loader/entries/arch.conf

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

# Setup folders
mkdir -p "/mnt/home/$USER"
rm -rf "/mnt/home/$USER/{Books,Documents,Pictures,Videos,Music}"

mkdir -p "/mnt/mnt/docs/$USER"
arch-chroot /mnt chown -R $USER:$USER "/mnt/docs/$USER"
arch-chroot /mnt chmod -R u=rwx,g=,o= "/mnt/docs/$USER"

mkdir -p "/mnt/mnt/media/$USER/{Books,Pictures,Videos,Music}"
arch-chroot /mnt chown -R $USER:$USER "/mnt/media/$USER"
arch-chroot /mnt chmod -R u=rwx,g=,o= "/mnt/media/$USER"

arch-chroot /mnt ln -s "/mnt/docs/$USER" "/home/$USER/Documents"
arch-chroot /mnt chown -h $USER:$USER "/home/$USER/Documents"

arch-chroot /mnt ln -s "/mnt/media/$USER/Books" "/home/$USER/Books"
arch-chroot /mnt chown -h $USER:$USER "/home/$USER/Books"

arch-chroot /mnt ln -s "/mnt/media/$USER/Pictures" "/home/$USER/Pictures"
arch-chroot /mnt chown -h $USER:$USER "/home/$USER/Pictures"

arch-chroot /mnt ln -s "/mnt/media/$USER/Videos" "/home/$USER/Videos"
arch-chroot /mnt chown -h $USER:$USER "/home/$USER/Videos"

arch-chroot /mnt ln -s "/mnt/media/$USER/Music" "/home/$USER/Music"
arch-chroot /mnt chown -h $USER:$USER "/home/$USER/Music"

# Generate ssh key
arch-chroot /mnt sudo -u "$USER" ssh-keygen -f "/home/$USER/.ssh/id_rsa" -N "$ssh_password"

# Copy the dotfiles folder into the new install
SCRIPT_PATH=$(realpath "$0")
DOTFILES_PATH=$(dirname "$SCRIPT_PATH" | xargs dirname)
mkdir -p "/mnt/home/$USER/.config"
arch-chroot /mnt chown -R $USER:$USER  "/home/$USER/.config"
rsync -arv --exclude old "$DOTFILES_PATH" "/mnt/home/$USER/.config/"
arch-chroot /mnt chown -R $USER:$USER  "/home/$USER/.config/dotfiles"
