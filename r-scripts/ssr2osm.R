# Lesing av data frå SSR (Sentralt stadnamnregister)
# og konvertering til OSM-format.
#
# Laga av Karl Ove Hufthammer <karl@huftis.org>.

# Nødvendige pakkar
library(rjson) # Lesing av JSON-data
library(plyr)  # Enkel datamassering

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors=FALSE)

# Les inn data
# Last først ned filene frå http://data.kartverket.no/stedsnavn/GeoJSON/Fylker/
d=fromJSON(file="20_finnmark_stedsnavn.geojson")[[2]]
length(d) # Kor mange oppføringar

# Gjer om dataa til ei dataramme (litt komplisert, men går ganske kjapt)
d2=lapply(d, unlist)
d3=do.call(rbind, d2)
d3=as.data.frame(d3)

# Fjern forledd i kolonnenamn
names(d3)=gsub(".*\\.","", names(d3))

# Sjå berre på gyldige skrivemåtar (vedtatt, godkjent, samlevedtak eller privat),
# jf. http://www.statkart.no/kart/stedsnavn/sentralt-stadnamnregister-ssr/saksbehandlingsstatus-for-skrivematen/
d3=subset(d3, skr_snskrstat %in% c("V","G","S","P"))        

# Sjå kjapt på dataa
head(d3)

# Aktuelle variablar
aktvar=c("enh_ssrobj_id", "skr_snskrstat", "enh_snavn", "enh_snspraak", "skr_sndato", 
         "nty_gruppenr", "enh_navntype", "enh_komm", "enh_ssr_id", "coordinates1", "coordinates2")

# Sjå berre på aktuelle variablar
d3=d3[aktvar]

# Gjer om tal til tal o.l.
d3=colwise(type.convert, na.strings="", as.is=TRUE)(d3)

# Fjern veg- og gatenamn (Kartverket styrer ikkje namna her)
d3=subset(d3, enh_navntype!=140 )

# Reformater datoar til ISO 8601-format
# (stolar på Kartverket, sjølv om nokre datoar er langt inni framtida …)
d3$skr_sndato=as.Date(as.character(d3$skr_sndato), format="%Y%m%d")

# Sorter etter ID, dato (nyaste vedtak først) og språk
# (seinare programkode antar sorterte data)
d3=arrange(d3, enh_ssrobj_id, desc(skr_sndato), enh_snspraak, enh_ssr_id)

# Sjå kjapt på dataa igjen
head(d3)


# Lag OSM-data basert på eit namneobjekt (enh_ssr_objid)
# Langt frå optimert eller elegant kode!
lagosm=function(d) {
  # Legg til data som er felles for alle namnevariantane
  res=data.frame(no_kartverket_ssr.objid=d$enh_ssrobj_id[1],
                no_kartverket_ssr.date=d$skr_sndato[1],
                no_kartverket_ssr.url=paste0("http://faktaark.statkart.no/SSRFakta/faktaarkfraobjektid?enhet=", d$enh_ssr_id[1]))
  # Legg til «name» og «alt_name» for kvart språk
  ddply(d, .(enh_snspraak), function(d) {
    namn=unique(d$enh_snavn) # Berre unike namn (av og til er same namn med fleire gongar, eks. enh_ssrobj_id 77153)
    res[paste0(c("name.", "alt_name."), d$enh_snspraak[1])] <<- namn[1:2]
  })
  res
}


# Hent ut ein (eksempel)kommune
komm=subset(d3, enh_komm==2003)

# Lag OSM-data for alle namneobjekta i den aktuelle kommunen
res=ddply(komm, .(enh_ssrobj_id), lagosm, .progress="text")

# Rett opp språkkodar (Kartverket brukar ustandard kodar for dei samiske språka, ikkje ISO 639)
names(res)=gsub("\\.SN", ".se", names(res))  # Nordsamisk
names(res)=gsub("\\.SL", ".smj", names(res)) # Lulesamisk
names(res)=gsub("\\.SS", ".sma", names(res)) # Sørsamisk
names(res)=gsub("\\.NO", "", names(res))     # Norsk (treng ikkje prefiks)
names(res)=tolower(names(res))               # Språkkodar om til små bokstavar

# Sjå på nokre av namna
head(res)

# Gjenstår: Lagra i passande format (hugs å fiksa «_» og «.» til «-» og «:» i variabelnamn) ...
