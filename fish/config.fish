alias ep="nvim ~/.config/fish/config.fish"
alias rp="source ~/.config/fish/config.fish"

alias d="cd ~/dev"
alias d2="cd ~/_dev"
alias ipad="cd ~/dev/iPadApp"
alias admin="cd ~/dev/BespokeMenus"
alias admin2="cd ~/dev/AdminSiteV2"
alias webp="cd ~/dev/WebPdfRenderer"
alias papi="cd ~/dev/ProductAPI"
alias papi-run="cd ~/dev/ProductAPI; and docker start product_api_db; and export DATABASE_URL=postgres://dev:dev@localhost:5432/products; and  npm run dev"
alias papi-db="docker exec -it product_api_db psql -U dev -d products"
alias rndebug="open \"rndebugger://set-debugger-loc?host=localhost&port=8081\""

set VISUAL /usr/local/bin/nvim

### PATH ###

set default_path /Users/user/.fastlane/bin /Users/user/.nvm/versions/node/v8.11.4/bin /Users/user/.fzf/bin /usr/bin /bin /usr/sbin /sbin
set homebrew /usr/local/bin
set -gx PATH $homebrew $default_path
# set -g -x PATH /usr/local $PATH
