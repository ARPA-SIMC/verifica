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
("postgresql://myuser:mypassword@radicchio-smr/mydb")

## Settaggi iniziali

Nel proprio profile (es file .bash_profile nella propria $HOME) e'
necessario impostare:

export EDITOR=emacs (o l'editor che si preferisce)
Inoltre e' necessario che sia definita la variabile $SCRATCH.

Si utilizza la variabile di sistema $VERSHARE.
Nella directory $VERSHARE si trovano i template delle namelist e dei file profile
che vengono usati nella verifica

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

Portarsi nella propria propria directory di lavoro. Qui va creato il file 
profile_nomemodello
che contiene il valore del parametro da verificare secondo la codifica grib.
Si possono trovare esempi in $VERSHARE. Una documentazione piuttosto vecchia
(ma meglio di niente) si trova nel file spiega_GRIBARCH.txt:
 ce=identificativo del centro di emissione (ksec1(2))
 ct=numero della versione della tabella variabili (ksec1(1))
 gp=processo generatore (ksec1(3))
 iv$par=codice grib del parametro (ksec1(6), vedi versione $ct della tabella 2)
Prima di iniziare la verifica bisogna assicurasi che il proprio parametro sia
presente nel file profile_nomemodello.
Altre indicazioni utili alla scrittura dei profile e della namelist si trovano
in fondo a questo README.

 0. Cancellazione del database

 Se si vuole lavorare su un database vuoto e' opportuno cancellarlo
 completamente prima di iniziare. Cio' si puo' fare con il comando
 `dbadb wipe`.

 1. Caricamento dei dati di riferimento

 Ciascun utente e' responsabile di crearsi le script ed i codici per
 caricare i dati di riferimento che intende usare.

 2. Caricamento dei dati da verificare

  Ciascun utente e' responsabile di crearsi le script ed i codici per
  caricare i dati da verificare che intende usare.

 3. Impostazione dei parametri della verifica

  Per l'impostazione dei parametri della verifica di lancia:

    * `ver_prepara_nml.sh`

  che apre in editor un file dove si scelgono i parametri della verifica
  E' possibile verificare tanto i forecast dei modelli quanto le loro analisi.
   
 4. Calcolo degli scores

  Gli score vengono calcolati eseguendo:

    * `ver_scores.sh`

  per gli scores di un singolo run (o di ciascun membro di un ensemble)
  oppure

    * `ver_scores_prob.sh`

  per gli scores di un ensemble.
  Per questo, deve esistere nella directory di lavoro un file chiamato pesi.nml
  (a carico dell'utente). I risultati si trovano nella directory di lavoro.

  I risultati sono file ASCII dal nome intuitivo.


### codice B della variabile ###

  Per sapere il codice B della variabile che si intende verificare, si
  guardi la tabella delle variabili in [36]DballeTabelle.
  Quando si fa la verifica per soglie, i valori delle soglie devono essere
  espressi nell'unita' di misura della variabile stessa, che e' quella
  specificata nella tabella DballeTabelle.

### reportobs e reportpre ###

Ciascun oggetto che entra nel database appartiene ad un tipo report,
identificato da un codice report e da un mnemonico report (una stringa) che si consiglia
di scegliere in modo "regolare". Tali report identificano in modo univoco i vari
previsti ed osservati caricati su un database temporaneo: si possono caricare prima
gli osservati, poi i previsti di 3 diversi modelli (con 3 diversi tipo report) e poi
effettuare le 3 verifiche in sequenza senza mai svuotare il database.
Ad esempio gli osservati puntuali di temperatura potrebbero avere il reportobs="oss",
mentre le precipitazioni osservate medie una una box di 0.2 x 0.2 gradi potrebbero
avere il reportobs="ossmed02".
Per i previsti, si consiglia di porre la prima parte di reportpre pari a $model (il
nome del modello), seguito dal tipo di elaborazione. Ad esempio:

reportpre (fino a 20 caratteri)

  * fino a 10 caratteri per la descrizione del modello

  * 5 caratteri per la descrizione del tipo di elaborazione
  spnpo (station point nearest point) per interpolazione punto piu vicino
  spbil per interpolazione bilineare
  med02 per il valore medio su box di 0.2 x 0.2 gradi

  * 5 caratteri per l'ensemble
  el001, el012, el111

Per cui reportpre potrebbe essere uguale a:
cosmo2Ispnpo
c2ireanamed02
c2iepsmed02el001

