FROM archlinux:latest

RUN pacman -Syu --noconfirm sudo base-devel
RUN echo "sandbox" > /etc/hostname
RUN useradd --create-home --shell /bin/bash --groups wheel tester \
  && sed -i '/%wheel ALL=(ALL:ALL) NOPASSWD: ALL/s/^#//g' /etc/sudoers

WORKDIR /home/tester
USER tester

COPY --chown=tester:tester . /home/tester/.config/dotfiles/

RUN /home/tester/.config/dotfiles/migrate.sh run

ENTRYPOINT ["/bin/zsh"]
