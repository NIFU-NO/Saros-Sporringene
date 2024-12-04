# Sjekkliste for nye gjennomføringer (kun post-datavask)

## Generer utkast straks surveyen er lukket og dataene vasket (noe kan gjøres parallelt):

01  Åpne RStudio (utenfor et prosjekt). Installer saros og saros.base fra github:
    * `devtools::install_github(c("saros", "saros.base", "saros.utils"))`
    
02  Åpne RStudio-prosjektet Sporringene.Rproj

03  Åpne og kjør 000_initialize_project.R

04  Åpne 001_configure_report.R
    * Endre `params$cycle <- "2024H"` til inneværende år og kjør kun den første linjen. H=høst, V=vår, osv
    * Ikke kjør for-loopen enda.
    * For å teste ut stegene 4-6, kjør `params$cycle <- "20XXX"` (der du bytter ut XXX med det som gjelder)

05  Kopier 01_script/002_specify_report_cycle_params_for_20XXX.R for nåværende år og endre parametrene.

06  Ved hjelp av 003_get_report_cycle_paths.R, sjekk at filene og mappene det pekes til ligger der de skal ligge (ikke endre i R-fila, skal være likt oppsett hvert år)

07  Kopier 01_script/200_prep_data_for_20XXX.R og tilpass som nødvendig.
    * Optimalt sett er alt allerede vasket.
    * Uansett må objekter samsvare med hva som er i dette skriptet.
    * Alle variabler som kan bli brukt må en gyldig variabellabel (gjelder også datasett for kapittel 2).

08  OBS: Sjekk at det ikke ligger noe "menneskeskrevet" i mappen du skal generere utkast i (ev ta kopi/bytt navn om du er usikker).

09  Kjør loopen i 001_configure_report.R, korriger ev. feil og sjekk at utkastene blir generert.
    * Hvorfor ikke bare kjøre skriptene 000, 002, 003, 200, 900 "manuelt"? Fordi det tvinger en til å sørge for at alt fungerer neste gang man skal gjøre dette, så man ikke blir fristet til quick-fix-snarveier 

10  Omdøp _quarto.yaml til quarto.yaml mens man jobber i enkeltkapitler, ellers skal Quarto prøve å generere alt for deg...

11  Gå inn på et par av qmd-utkastene og sjekk at det kompilerer riktig.

## Kapittel 2

20	Kommer

## Generer DOCX for oversending kvalitetssikring (dette kan hver kapittelforfatter gjøre selv)

30  Trykk på liten pil ved Render-knappen, velg Render DOCX. Dersom man skal få de ut i NIFU-malen må nifudocx-utvidelsen være installert: 
    * Åpne Terminal (ved siden av konsollen). 
    * Naviger deg til mappen hvor utkastet ligger, med kommandoen `cd "Rapporter/2024V"`.
    * Kjør `quarto add NIFU-NO/nifudocx` og velg Yes, vent litt, så Yes igjen og så No.

## Sammenstill og publiser komplett rapport

40

## Ofte stilte spørsmål:
* Hva om vi trenger å oppdatere datasettet (f.eks. variabel-labels, noen som skulle filtreres bort, etc)? Må alt gjøres på nytt?
  * Frykt ikke, det er minst tre alternative løsninger, etter du har oppdatert datasettet og lagret dette (se paths$data$saros_ready i 003_get_report_cycle_paths og 200_prep_data_for_XXXXX):
    a) Kjør 900 på nytt, men i en egen mappe. Fisk så ut kapitteldatasettene og erstatt de du har fra før.
    b) Lag nye kapitteldatasett manuelt med hjelp av kapitteloversikten og en loop. Se 209_backup_solutions.R
    c) Omgjør starten på alle qmd-kapitlene slik at de alle peker til samme store fil, og så gjør filtrering og selektering i hver qmd-fil 
      Ulemper: Tar ekstra tid å kompilere hver gang. Alle må jobbe i "samme mappestruktur" eller i hvert fall oppdatere filbanen til datasettet. Og en ørliten risiko for at noen roter til det felles datasettet og dermed ødelegger for andre.
