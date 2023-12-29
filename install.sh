echo "Installer is under development..."

OS="$(uname)" # Linux, Darwin
ARCH="$(uname -m)" # arm64, x86_64, aarch64
case "${SHELL}" in
  */bash*)
    RC="${HOME}/.bashrc"
    ;;
  */zsh*)
    RC="${ZDOTDIR:-"${HOME}"}/.zshrc"
    ;;
  */fish*)
    RC="${HOME}/.config/fish/config.fish"
    ;;
  *)
    RC="${ENV:-"${HOME}/.profile"}"
    ;;
esac

if ! [[ -x "$(command -v moa)" ]]
then
  EXPORT="export PATH=~/moa/bin:\$PATH"
  if ! fgrep -q "export PATH=~/moa/bin:" "${RC}"
  then
    [ -f ${RC} ] && echo "${EXPORT} # moa installer appended" >> "${RC}"
    export PATH=~/moa/bin:$PATH
  fi
fi
