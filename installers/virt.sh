#!/usr/bin/env bash

set -ex

# Script for installing my arch linux setup onto virt manager
#
# Requries UEFI to be enabled
#
# Steps for virt-manager:
# 1. Check "Customize configuration before install"
# 2. Ensure that UEFI is enabled before creating
# 3. In the settings of the VM, click Add Hardware
# 4. Select filesystem and enter both the source and target paths
# 5. Once in the shell run `mount -t 9p -o trans=virtio <target> <some local dir>

# Prompts:
# - Confirm luks with YES
# - Password for root luks
# - Password for root luks
# - Password for root luks
# - Confirm luks with YES
# - Password for extra luks
# - Password for extra luks
# - Password for extra luks
# - Password for root user
# - Password for root user
# - Password for david user
# - Password for david user


# Update the system clock
timedatectl set-ntp true

# Partition the disks
parted --script /dev/vda mklabel gpt
parted --script /dev/vda mkpart boot fat32 1MiB 513MiB
parted --script /dev/vda set 1 esp on
parted --script /dev/vda mkpart luks 513MiB 100%

parted --script /dev/vdb mklabel gpt
parted --script /dev/vdb mkpart luks 1MiB 100%

# Setup luks/lvm
cryptsetup luksFormat --type luks /dev/vda2
cryptsetup open /dev/vda2 mainlvm
pvcreate /dev/mapper/mainlvm
vgcreate main /dev/mapper/mainlvm
lvcreate -l 100%FREE main -n root

cryptsetup luksFormat --type luks /dev/vdb1
cryptsetup open /dev/vdb1 extralvm
pvcreate /dev/mapper/extralvm
vgcreate extra /dev/mapper/extralvm
lvcreate -l 100%FREE extra -n main

# Format the partitions
mkfs.fat -F32 -n boot /dev/vda1
mkfs.ext4 -L root /dev/main/root
mkfs.ext4 -L extra /dev/extra/main

# Mount the file systems
mount /dev/main/root /mnt
mkdir -p /mnt/boot
mkdir -p /mnt/mnt/extra
mount /dev/vda1 /mnt/boot
mount /dev/extra/main /mnt/mnt/extra

# Install essential packages
pacstrap /mnt base base-devel linux linux-firmware neovim networkmanager lvm2

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
HOSTNAME="virt"
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
  echo "main    UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/vda2)   none"
  echo "extra   UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/vdb1)   none"
} >> /mnt/etc/crypttab.initramfs

sed -i 's/^HOOKS=.*/HOOKS=\(base systemd autodetect keyboard modconf block sd-encrypt lvm2 filesystems fsck\)/' /mnt/etc/mkinitcpio.conf
arch-chroot /mnt mkinitcpio -P

# Prompt to set root password
echo "Set the password for root"
arch-chroot /mnt passwd

# Configure boot loader
arch-chroot /mnt bootctl install
{
  echo "title     Arch Linux"
  echo "linux     /vmlinuz-linux"
  echo "initrd    /initramfs-linux.img"
  echo "options   cryptdevice=UUID=$(arch-chroot /mnt blkid -s UUID -o value /dev/vda2):cryptlvm  root=/dev/main/root rw"
} >> /mnt/boot/loader/entries/arch.conf

# Create main user
USER="david"
arch-chroot /mnt useradd --create-home --groups wheel --shell /bin/bash $USER
arch-chroot /mnt pacman -S --noconfirm --needed sudo
sed -i '/%wheel ALL=(ALL:ALL) ALL/s/^#//g' /mnt/etc/sudoers
echo "Set the password for $USER"
arch-chroot /mnt passwd $USER

# Enable automatic login
mkdir -p /mnt/etc/systemd/system/getty@tty1.service.d
{
  echo "[Service]"
  echo "ExecStart="
  echo "ExecStart=-/usr/bin/agetty --autologin $USER --noclear %I \$TERM"
} >> /mnt/etc/systemd/system/getty@tty1.service.d/override.conf

# Copy the dotfiles folder into the new install
SCRIPT_PATH=$(realpath "$0")
DOTFILES_PATH=$(dirname "$SCRIPT_PATH" | xargs dirname)
mkdir -p "/mnt/home/$USER/.config"
arch-chroot /mnt chown -R $USER:$USER  "/home/$USER/.config"
rsync -arv --exclude old "$DOTFILES_PATH" "/mnt/home/$USER/.config/"
arch-chroot /mnt chown -R $USER:$USER  "/home/$USER/.config/dotfiles"
