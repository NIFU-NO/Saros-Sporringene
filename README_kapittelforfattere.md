# Bruksanvisning for kapittelforfattere

* Du må synkronisere General-kanalen til din lokale maskin og jobbe derfra i RStudio.
  * Du må også synkronisere Metode-teamets General-kanal (dette teamet er uansett veldig nyttig for deling av tips og triks).
* Åpne qmd-fila i kapittelmappen direkte fra RStudio.
  * Unngå å bruke .Rproj-fila i Saros-hovedmappen. Det vil trolig dra deg inn i mer kompleks kompilering av hele nettstedet. 
* Skriv i konsollen og trykk enter:
  * `devtools::install_github("NIFU-NO/saros")`
  * Bør gjøres hvert halvår
* Det er to filer per kapittel, ett for "ikke-fylker" og ett for fylker. (forskjellen er at det for fylker er antall og ikke prosenter)
  * Du skal kunne kopiere over såkalte chunks (kode som lager figurer) fra _fylke til hovedfilen uten problemer. Bare sørg for at du får med alt inklusive ```{r} til og med ```
* Det ligger "støttemapper" i kapittelmappen for Word-dokumenter som skal til og kommer fra Udir og kvalitetssikrer. Din jobb å plassere dem i "til ..." og melde prosjektleder.
* Trykk `Alt + Shift + L` når du åpner qmd-filen. Da kollapser man all kode-chunks. 
* Kjør så en og en chunk ved behov (grønn pil til høyre på linjen, i starten av chunken) 
* Skriv funnene før(?) chunken.
* Feilmeldinger:
  * `could not find function "makeme"`: Se punktet ovenfor om å installere saros-pakken fra Github.
  * `Unequal variables. All variables must share at least one common category.`: dep-variablene dine er ikke like nok i svarkategoriene. Fjern den som ikke passer inn.
  * Feilmelding omhandler `all_of(indeps)`: Dette gjelder signifikanstest-tabellene helt nederst. Fjern de uavhengige variablene den klager på, fra indeps-listen og prøv igjen. Ev. kommenter ut hele signifikanstabellen, skal ikke i ferdig rapport uansett.
  * `Undefined columns selected`: 
    * Sjekk at kombinasjonen av avhengig og uavhengig variabel gir mening. 
    * Feilen finner du på linjene som det står nederst i feilmeldingen.
    * Med saros 1.2 og nyere kan det være at du får forslag til en figur som ikke lar seg lage (feilmelding: `""`) fordi spørsmålet er kun stilt til skoleleder, og den uavhengige variabelen kun er ment for skoleeiere. (eksempel fra 2024H: Q3.11 vs kstør_kom)
    * Skal etter hvert prøve enten å gi en tydeligere feilmelding, eller at saros bare ikke produserer noe som helst (usynlig output)


## Når du har kommet litt i gang
* I chunkene finnes noen linjer som er kommentert ut (begynner på # og fortsetter med link, link_plot, x). Disse ignorerer du, men ikke fjern det. 
  * De linjene lager nedlastbare varianter av figurene som PNG og XLSX for nettstedet. :)
  * Lov å teste ut, men vil skape unødvendig mange ekstra filer i kapittelmappen din.
* Prøver du å rendre/koompilere alt samtidig vil du trolig få opp en feilmelding pga signifikanstest-tabellene helt nederst, som gjør noen antagelser om at alle uavhengige variabler er tilstede. 
  a) Enten kan du finne ut hvilke som fungerer og ikke og fjerne disse fra indeps-listen, eller 
  b) Ikke forhold deg til tabellen, bare kommenter ut hele chunken - for den skal jo uansett ikke med i rapporten.
* Kryssreferanser til figurer: "@Fig-min-figur viser at ...". "Som vi ser i @fig-min-figur2 så ..."
