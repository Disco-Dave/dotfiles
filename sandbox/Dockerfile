FROM archlinux:latest

RUN pacman -Syu --noconfirm sudo

RUN useradd --create-home --shell /bin/bash --groups wheel testuser \
  && sed -i '/%wheel ALL=(ALL) NOPASSWD: ALL/s/^#//g' /etc/sudoers

WORKDIR /home/testuser
USER testuser

COPY --chown=testuser:testuser . /home/testuser/.config/dotfiles/

ENV ENVIRONMENT "sandbox"

ENTRYPOINT ["/home/testuser/.config/dotfiles/sandbox/entrypoint.sh"]
