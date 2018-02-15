library(httr)
library(gmailr)
# oauth_endpoints("google")
# myapp <- oauth_app("google",key = "613989698689-cpnj6ud3hg6c56i29b62tr71rj2nujb9.apps.googleusercontent.com",secret = "CYsTut0sls78boDs7J3XLPTL")
# google_token <- oauth2.0_token(oauth_endpoints("google"), myapp,scope = "https://www.googleapis.com/auth/gmail.readonly")
# req <- GET("https://www.googleapis.com/gmail/v1/users/rafaelmenmell@gmail.com/messages?q=from:sanidadambiental.polen@salud.madrid.org&maxResults=10",config(token = google_token))
# stop_for_status(req)
# content(req)
# #para cada uno me descargo los attachments
# nmensajes <- length(content(req)$messages)
# for (n in nmensajes){
#   id <- content(req)$messages[[n]]$id
#   mensaje <- GET(sprintf("https://www.googleapis.com/gmail/v1/users/rafaelmenmell@gmail.com/messages/%s?",id),config(token = google_token))
#   stop_for_status(mensaje)
#   partes <- length(content(mensaje)$payload$parts)
#   for (i in 1:partes){
#     if (attachmentId in names(content(mensaje)$payload$parts[[i]]$body)){
#       idatacha <- content(mensaje)$payload$parts[[2]]$body$attachmentId
#       a <- GET(sprintf("https://www.googleapis.com/gmail/v1/users/rafaelmenmell@gmail.com/messages/%s/attachments/%s",id,idatacha),config(token = google_token))
#       download.file(a,destfile="attach/kk1.pdf")
#     }
#   }
# }

saveAttachaments = function(id,folder)
{
  save_attachments(message(id),path=folder)
}

gmail_auth(secret = "CYsTut0sls78boDs7J3XLPTL",scope = "read_only",id = "613989698689-cpnj6ud3hg6c56i29b62tr71rj2nujb9.apps.googleusercontent.com")
mensajes <- messages("from:sanidadambiental.polen@salud.madrid.org",700)
id.mensajes <- id(mensajes)
for (i in 1:length(id.mensajes)){
  date <- as.POSIXct(as.numeric(message(id(mensajes)[i])$internalDate)/1000,origin="1970-01-01")
  date <- as.character(date,format="%Y%m%d")
  print(date)
  folder <- sprintf("attach/%s",date)
  dir.create(folder,showWarnings = FALSE)
  saveAttachaments(id.mensajes[i],folder)
}