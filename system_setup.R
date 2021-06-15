# Use custom fonts in shiny
dir.create('~/.fonts')
file.copy("NotoSansMonoCJKtc.otf", "~/.fonts")
system('fc-cache -f ~/.fonts')

