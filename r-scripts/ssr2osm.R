# Lesing av data frå SSR (Sentralt stadnamnregister)
# og konvertering til OSM-format.
#
# Laga av Karl Ove Hufthammer <karl@huftis.org>.

# Variabler for input og output
ssr_geojson="../data/stedsnavn.geojson"
osm_csv="~/test.csv"


# Nødvendige pakkar
library(jsonlite) # Lesing av JSON-data
library(plyr)     # Enkel datamassering
library(stringr)  # Avansert strenghandtering

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors=FALSE)

# Les inn data
# Last først ned filene frå http://data.kartverket.no/download/content/stedsnavn-ssr-wgs84-geojson
d=fromJSON(txt=ssr_geojson)$features
length(d) # Kor mange oppføringar

# Legg koordinatar til resten av dataa
d2=d$properties
d2[c("coordinates1","coordinates2")]=ldply(d$geometry$coordinates)

# Sjå berre på gyldige skrivemåtar (vedtatt, godkjent, samlevedtak eller privat),
# jf. http://www.statkart.no/kart/stedsnavn/sentralt-stadnamnregister-ssr/saksbehandlingsstatus-for-skrivematen/
d3=subset(d2, skr_snskrstat %in% c("V","G","S","P"))

# Fjern gamle objekt (for å spara litt minne)
rm(d, d2)
gc()

# Sjå kjapt på dataa
head(d3)

# Aktuelle variablar
aktvar=c("enh_ssrobj_id", "skr_snskrstat", "enh_snavn", "enh_snspraak", "skr_sndato", 
         "nty_gruppenr", "enh_navntype", "enh_komm", "enh_ssr_id", "coordinates1", "coordinates2")

# Sjå berre på aktuelle variablar
d3=d3[aktvar]

# Fjern veg- og gatenamn (Kartverket styrer ikkje namna her)
d3=subset(d3, enh_navntype!=140)

# Reformater datoar til ISO 8601-format
# (stolar på Kartverket, sjølv om nokre datoar er langt inni framtida …)
d3$skr_sndato=as.Date(as.character(d3$skr_sndato), format="%Y%m%d")

# Hent inn info om objekttypar
objtypar=read.csv("../data/objekttypar.csv", sep=",", na.strings="")[c("enh_navntype","osm")]
head(objtypar)

# Legg objekttypeinfo til SSR-dataa
d3=join(d3, objtypar, by="enh_navntype", type="left", match="first")

# Sjå vidare berre på objekt som har OSM-taggar definert
d3=subset(d3, !is.na(osm))

# Del opp samansette taggar, og fjern mellomrom før/etter tekstbitane
taggar=strsplit(d3$osm, ";", fixed=TRUE)
taggar=sapply(taggar, str_trim)

# Del opp taggar i tagg + verdi
tagg.verdi=lapply(taggar, strsplit, split="=")

# Oversikt over taggnamn brukte
taggnamn=unique(gsub("([^=*])=.*", "\\1", d3$osm))
sort(taggnamn)

# Gjer om til liste over namngjevne verdiar (namnet == taggen),
# eitt element for kvar rad
hent.verdiar=function(rad) {
  namn=sapply(rad, function(x) x[1])
  verdiar=sapply(rad, function(x) x[2])
  names(verdiar)=namn
  verdiar
}
tagginfo.l=lapply(tagg.verdi, hent.verdiar)

# Lag dataramme med éi kolonne for kvar tagg
tagginfo.df=sapply(taggnamn, function(objtype) sapply(tagginfo.l, "[", objtype))

# Legg taggkolonnar til den opphavlege dataramma
d3=data.frame(d3, tagginfo.df)


# Sorter etter ID, dato (nyaste vedtak først) og språk
# (seinare programkode antar sorterte data)
# (må komma etter join()-kommandoen)
d3=arrange(d3, enh_ssrobj_id, desc(skr_sndato), enh_snspraak, enh_ssr_id)

# Sjå kjapt på dataa igjen
head(d3)


# Lag OSM-data basert på eit namneobjekt (enh_ssr_objid)
# Langt frå optimert eller elegant kode!
lagosm=function(d) {
  # Legg til data som er felles for alle namnevariantane
  res=data.frame(no_kartverket_ssr.objid=d$enh_ssrobj_id[1],
                no_kartverket_ssr.date=d$skr_sndato[1],
                no_kartverket_ssr.url=paste0("http://faktaark.statkart.no/SSRFakta/faktaarkfraobjektid?enhet=", d$enh_ssr_id[1]),
                longitude=d$coordinates1[1],
                latitude=d$coordinates2[1],
                d[1, taggnamn])
  # Legg til «name» og «alt_name» for kvart språk
  d_ply(d, .(enh_snspraak), function(d) {
    namn=unique(d$enh_snavn) # Berre unike namn (av og til er same namn med fleire gongar, eks. enh_ssrobj_id 77153)
    res[paste0(c("name.", "alt_name."), d$enh_snspraak[1])] <<- namn[1:2]
  })
  res
}


# Funksjonar for å laga klar og lagra OSM-data for kvar kommune

# Formater kommunenummer med fire siffer
d3$enh_komm=formatC(d3$enh_komm, flag="0", width=4)

# Funksjon for å laga klar og lagra OSM-data for
# eit gitt kommunedatasett (utdrag av d3)
lagra_kommune=function(komm) {
  # Lag OSM-data for alle namneobjekta i den aktuelle kommunen
  res=ddply(komm, .(enh_ssrobj_id), lagosm, .progress="none")
  
  # Rett opp språkkodar (Kartverket brukar ustandard kodar for dei samiske språka, ikkje ISO 639)
  names(res)=gsub("\\.SN", ".se", names(res))  # Nordsamisk
  names(res)=gsub("\\.SL", ".smj", names(res)) # Lulesamisk
  names(res)=gsub("\\.SS", ".sma", names(res)) # Sørsamisk
  names(res)=tolower(names(res))               # Språkkodar om til små bokstavar
  
  ## Sjå på nokre av namna
  #head(res)
  
  
  # Ikkje alle plassar har norske namn. Bruk det norske
  # namnet som «name»/«alt_name» dersom det finst,
  # ev. eitt av dei andre namna. (Ja, litt uelegant kode,
  # men fungerer kjapt og greitt.)
  
  # Namn og kolonnenummer på name-kolonnar
  namevar=unique(c("name.no", grep("name\\.", names(res), value=TRUE)))
  namekol=match(namevar, names(res))
  
  # Indeks til (første) kolonne som har namn (der norsk har førsteprioritet)
  name.ind=min(namekol)-1+apply(res[,namekol, drop=FALSE], 1, function(x) which.min(is.na(x)))       
  altname.ind=name.ind+1 # Bruk alltid «alt_name»-namnet som finst for språket som er brukt
                         # i «name» (sjå ssr_objid 320315 for eit eksempel på korfor)
  
  # Legg til name- og alt_name-kolonnar
  res$name=res[cbind(1:nrow(res),name.ind)]
  res$alt_name=res[cbind(1:nrow(res),altname.ind)]
  
  # Fjern kolonnar me ikkje (lenger) treng
  res=res[,!(names(res) %in% c("enh_ssrobj_id", "osm", "name.no", "alt_name.no"))]

  # Ikkje ta med kartverkspesifikke metadata (kommenter ev. ut)
  res=res[,!grepl("kartverket", names(res))]
  
  # Lag klar rette kolonnenamn
  kolnamn=gsub("no_kartverket_ssr", "no-kartverket-ssr", names(res))
  kolnamn=gsub("\\.", ":", kolnamn)
  
  # Lagra i CSV-format, for enkel bruk i JOSM og andre program
  utmappe="utdata/"
  if(!dir.exists(utmappe)) dir.create(utmappe)
  utfil=paste0(utmappe, komm$enh_komm[1], ".csv")
  write.table(res, file=utfil, col.names=kolnamn,
              sep=",", dec=".", na="", row.names=FALSE)
}

# For kvar kommune, lag klar data i OSM-format, og lagra
# som CSV-fil, for bruk i (for eksempel) JOSM
d_ply(d3, .(enh_komm), lagra_kommune, .progress="text")
