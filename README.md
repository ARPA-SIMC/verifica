[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=centos:7&label=centos7)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=centos:8&label=centos8)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=fedora:32&label=fedora32)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=fedora:33&label=fedora33)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=fedora:34&label=fedora34)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/verifica?branch=master&env=DOCKER_IMAGE=fedora:rawhide&label=fedorarawhide)](https://travis-ci.org/ARPA-SIMC/verifica)
[![Build Status](https://copr.fedorainfracloud.org/coprs/simc/stable/package/verifica/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/simc/stable/package/verifica/)

# Verifica

## Uso del pacchetto di verifica

Si puo' utilizzare da una qualunque macchina LINUX. La verifica si serve
di un database temporaneo sui cui vengono caricati tutti i dati e da cui
vengono poi letti per calcolare gli score.
Ciascuno puo' disporre di un database, vuoto all'inizio, indentificato
univocamente da nome, user e password.

## Settaggi iniziali

Nel proprio profile (es file .bash_profile nella propria $HOME) e'
necessario impostare:

export EDITOR=emacs (o l'editor che si preferisce)
Inoltre e' necessario che sia definita la variabile $SCRATCH.

Si utilizza la variabile di sistema $VERSHARE.

## Eseguire la verifica

Si lavora all'interno di una propria directory di lavoro. E' necessario
portarsi in questa directory per lanciare le varie script, perche' tutti i
file di appoggio, tutte le namelist e tutti i file dei risultati vengono
copiati o creati nella directory corrente.

Per eseguire la verifica e' necessario lanciare 4 script in sequenza:

 1. una per il caricamento dei dati di riferimento contro cui verificare
    (p.e. osservati) su database DB-ALL.e

 2. una per il caricamento dei dati da verificare (p.e. previsti da un
    modello) su database DB-ALL.e

 3. una per impostare i parametri della verifica

 4. una per il calcolo degli score (leggendo previsti e osservati da
    database DB-ALL.e)

Le prime due script (passo 1 e 2, per il caricamento di osservati e previsti)
sono al momento a carico dell'utente, che puo' usare a questo scopo la libsim.

Le script per i passi 3 e 4 si trovano in $PATH e cominciano per ver_.

E' possibile verificare solo le variabili riportate nel file
`$VERSHARE/griBlocale.txt` e solo i modelli per cui esiste nella stessa
directory un file chiamato profile_nomemodello. Prima di iniziare la
verifica bisogna assicurasi che il proprio parametro sia presente in
griBlocale e che il proprio modello abbia il suo profile, altrimenti e'
necessario aggiungerli direttamente nella propria directory di lavoro (->
vedere `aggiungimodello`).

 0. Cancellazione del database

 Se si vuole lavorare su un database vuoto e' opportuno cancellarlo
 completamente prima di iniziare. Cio' si puo' fare con il comando
 `dbadb wipe`.

 1. Caricamento dei dati di riferimento

 Ciascun utente e' responsabile di crearsi le script ed i codici per
 caricare i dati di riferimento che intende usare.
 Prima di fare qualunque altra cosa: controllare che il file `repinfo.csv`
 (che si trova in `/etc/dballe/`) contenga le informazioni relative ai report
 che si useranno. Tale file va copiato nella propria directory di lavoro.
 Se mancano i tipi report voluti si possono aggiungere a questa.

  * Caricamento di analisi in formato GRIB
     * E' possibile caricare campi analizzati in formato GRIB, scritti
       in coordinate ruotate e non.
   ATTENZIONE!!!
   E' prima necessario fare nella directory di lavoro un link al file
   contenente tali dati:
   `ln -s nome_file analisi.grib`
   ed anche un link al file di orografia appropriato (sulla stessa griglia!):
   `ln -s nome_file_orografia (p.e. $VERSHARE/orog_lmsmr2031.grib)
   orografia.grib`
   Poi si puo' lanciare la script:
     * `ver_scrivi_ana.sh`
   per avere caricati su database [34]DbAlle i campi analizzati contenuti nel
   file, localizzati sui punti griglia e con quota pari a quella specificata
   del grib di orografia dato.

 2. Caricamento dei dati da verificare

  Ciascun utente e' responsabile di crearsi le script ed i codici per
  caricare i dati da verificare che intende usare.

 3. Impostazione dei parametri della verifica

  Per l'impostazione dei parametri della verifica di lancia:

    * `ver_prepara_nml.sh`

  che apre in editor un file dove si scelgono il modello da verificare, la
  variabile, il livello, le scadenze. E' possibile verificare tanto i
  forecast dei modelli quanto le loro analisi, l'unica differenza e' la
  scadenza di previsione scelta.
   
 4. Calcolo degli scores

  Gli score vengono calcolati eseguendo:

    * `ver_scores.sh`

  per gli scores di un singolo run (o di ciascun membro di un ensemble)
  oppure

    * `ver_scores_prob.sh`

  per gli scores di un ensemble.
  Deve esistere nella directory di lavoro un file chiamato pesi.nml (a
  carico dell'utente). I risultati si trovano nella directory di lavoro.

  Per sapere il codice B della variabile che si intende verificare, si
  guardi la tabella delle variabili in [36]DballeTabelle. Come gia' detto,
  tale variabile deve trovarsi anche nel file griBlocale.txt, che si trova
  nella propria directory di lavoro. Quando si fa la verifica per soglie, i
  valori delle soglie devono essere espressi nell'unita' di misura della
  variabile stessa, che e' quella specificata nella tabella
  DballeTabelle.

## Sistemare i file di appoggio

 Preparazione del file `repinfo.csv`

Ciascun oggetto che entra nel database appartiene ad un tipo report,
identificato da un codice report e da un mnemonico report (il vecchio
descrittore). In repinfo.csv devono comparire le descrizioni dei tipi
report che si intendono usare:

  codice_report,mnemonico_report,descrizione
  libera,priorità,categoria,tabella-A

Il codice_report deve restare univoco solo per ogni sessione, quando si
cancella e si ricrea il database (p.e. usando scrivi_oss.bash) si puo'
modificare tutto. Il mnemonico_report (fino a 20 caratteri) e' vincolato
dalle regole usate per costruire il descrittore:

  * fino a 10 caratteri per la descrizione del modello ( il nome del
    dataset utilizzato in [38]XgribArch )

  * 5 caratteri per la descrizione del tipo di elaborazione

  * 5 caratteri per l'ensemble

Categoria e tabella-A non sono utilizzati al momento.

### Come aggiungere un modello da verificare

Se si vuole aggiungere un modello da verificare (verificare un modello non
previsto in griBlocale.txt o per cui non esiste il file
profile_nomemodello) bisogna:

  * creare il file profile_nomemodello con i codici grib del modello
    (vedere quelli esistenti in $VERSHARE per avere un esempio) nella
    propria directory di lavoro

 ce=identificativo del centro di emissione (ksec1(2))
 ct=numero della versione della tabella variabili (ksec1(1))
 gp=processo generatore (ksec1(3))
 if=fattore moltiplicativo delle precipitazioni se nel grib non sono gia' in millimetri (sparira')
 iv$par=codice grib del parametro (ksec1(6), vedi versione $ct della tabella 2)

  * aggiornare in locale il file griBlocale.txt, in cui e' scritta la
    corrispondenza tra il codice grib di un parametro ed il codice della
     Blocale del parametro all'interno di [39]DbAlle. Per conoscere il
    codice Blocale si veda la tabella variabili in DballeTabelle.

    || codice variabile (tre numeri: ce,ct,iv$par) || Blocale || a || b || mnemonico libero ||

 dove valore_in_database = a + b*valore_nel_grib. I valori di a e b devono quindi essere decisi in base all'unita' di misura che la variabile a nel grib ed a quello che ha in Blocale.
