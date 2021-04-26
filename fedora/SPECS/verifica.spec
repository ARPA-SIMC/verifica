Summary:       Verification software for ARPAE-SIMC products
Name:          verifica
Version:       5.0
Release:       1
License:       GPL
Group:         Applications/Meteo
URL:           https://github.com/arpa-simc/%{name}
Source:        https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{name}-%{version}-%{release}.tar.gz
BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: libtool
BuildRequires: gcc-gfortran
BuildRequires: eccodes-devel
BuildRequires: eccodes-simc
BuildRequires: libdballe-devel
BuildRequires: libdballef-devel
Requires: libdballef4
Requires: eccodes

%description
Tale pacchetto permette di effettuare una verifica oggettiva di vari
prodotti disponibili ad ARPAE-SIMC: previsioni effettuate dai modelli, sia
deterministici sia sistemi di ensemble, campi analizzati dai modelli,
campi derivati dal radar, prodotti dei modelli di qualita\'
dell\'aria. I campi da verificare devono essere in formato GRIB. La 
verifica viene effettuata contro un riferimento che puo\' essere
costituito da dati osservati su punti stazione (valori in punti
sparsi) o da campi analizzati (valori su grigliato regolare).
Il pacchetto si serve del database temporaneo DbAlle, dove i dati da
verificare vengono caricati insieme ai valori contro cui effettuare la
verifica e dal quale vengono poi estratti per calcolare una serie di
misure di errore.

%prep
rm -rf %{buildroot}
%setup -q -n %{name}-%{version}-%{release}
sh autogen.sh

%build

%configure FC=gfortran FCFLAGS="$RPM_OPT_FLAGS -I/usr/include/ -I%{_fmoddir}"

make

%install
[ "%{buildroot}" != / ] && rm -rf %{buildroot}
%makeinstall

%clean
[ "%{buildroot}" != / ] && rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
/etc/profile.d/verifica.sh

%dir %{_datadir}/verifica
%{_datadir}/verifica/*

%{_bindir}/*
%{_includedir}/*

%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so*

%changelog
* Mon Apr 26 2021 Daniele Branchini <dbranchini@arpae.it> - 5.0-1
- massive refactory for switching from gribex/libsmr to eccodes/libsim

* Mon Mar 4 2019 Daniele Branchini <dbranchini@arpae.it> - 4.4-1
- increased size of MNBOX to accomodate 5km grids
- rebuild on dballe 8

* Mon Dec 10 2018 Daniele Branchini <dbranchini@arpae.it> - 4.3-2
- added grib_api flags and dependencies

* Tue May 15 2018 Daniele Branchini <dbranchini@arpae.it> - 4.3-1
- migrated to github, adding travis and copr automation
- increased the size of MNBOX from 2500000 to 5000000

* Tue Mar 27 2018 Daniele Branchini <dbranchini@arpae.it> - 4.2-112%{dist}
- aggiornamenti e correzione bug per dballe v7

* Thu Dec 21 2017 Daniele Branchini <dbranchini@arpae.it> - 4.1-107%{dist}
- rimosso anaid
- modifiche per supporto db dballe v7

* Wed Jul 13 2016 Daniele Branchini <dbranchini@arpae.it> - 4.0-103%{dist}
- aggiornamento per nuovo interfaccia fortran dballe

* Fri Mar 14 2014 root <root@wanda.metarpa> - 3.9-90%{dist}
- forzato dato scalare ad essere >= 0; corretto calcolo RPSSD

* Thu Nov  7 2013 root <root@wanda.metarpa> - 3.7-90%{dist}
- allungata dimensione nome database

* Wed Oct  9 2013 root <root@wanda.metarpa> - 3.7-90%{dist}
- correction rps and rpss

* Mon Jun 10 2013 root <root@wanda.metarpa> - 3.6-90%{dist}
- aggiornamento per dballe6

* Wed Oct 13 2010 root <root@pigna> - 3.0-77
- sistemate dipendenze dballe

* Mon Jul 20 2009 root <root@wanda.metarpa> - 2.15-1
- aumentata dimensione array per grib modello

* Mon May 25 2009 root <root@wanda.metarpa> - 2.14-1
- corretto baco

* Wed May 20 2009 root <root@wanda.metarpa> - 2.13-1
- aggiunto RPSSD e corretto baco caricamento umrel regioni

* Mon May 18 2009 root <root@wanda.metarpa> - 2.12-1
- create librerie

* Fri May  8 2009 root <root@wanda.metarpa> - 2.11-1
- correzioni e calcolo spread/errore

* Wed Apr 15 2009 root <root@wanda.metarpa> - 2.9-1
- aggiunto score BSSD

* Fri Mar 20 2009 root <root@wanda.metarpa> - 2.8-1
- dichiarate alcune variabili

* Tue Mar  3 2009 root <root@wanda.metarpa> - 2.7-5
- aggiunta correzione quota e corretti bachi

* Wed Jul 23 2008 root <root@wanda.metarpa> - 2.5-5
- corretti bachi

* Tue Jul 22 2008 root <root@wanda.metarpa> - 2.5-1
- ottimizzazione ver_score_prob

* Thu Jul 17 2008 root <root@wanda.metarpa> - 2.4-1
- correzione baco

* Wed Jun 18 2008 root <root@wanda.metarpa> - 2.3-1
- correzione bachi + aggiunta correzione differenza di quota e td nulla

* Thu May 29 2008 root <root@wanda.metarpa> - 2.2-1
- correzione baco

* Thu May 29 2008 root <root@wanda.metarpa> - 2.1-1
- correzione bachi per passaggio a dballe4.0

* Wed Mar 12 2008 root <root@localhost.localdomain> - 2.0-1
- aggiornamento per dballe4.0, livelli e scadenze

* Mon Jan 21 2008 root <root@wanda.metarpa> - 1.22-1
- gestita analisi di precipitazione e modificato formato lettura in ver_leggidati_euro

* Fri Jul 13 2007 root <root@wanda.metarpa> - 1.21-1
- sistemati output statistici

* Tue Jul 10 2007 root <root@wanda.metarpa> - 1.19-1
- aggioramento codici verifica direzione vento e onde

* Fri Jun 22 2007 root <root@wanda.metarpa> - 1.18-1
- aggiornamenti/correzioni per dballe 3.6 + sviluppo

* Wed Apr  4 2007 root <root@wanda.metarpa> - 1.17-2
- corretti bachi verifica analisi

* Tue Apr  3 2007 root <root@wanda.metarpa> - 1.16-1
- aggiornamento per dballe3

* Thu Jan 18 2007 root <root@wanda.metarpa> - 1.13-2
- correzione baco

* Wed Sep  6 2006 root <root@wanda.metarpa> - 1.13-2
- corretta gestione block

* Fri Jul 14 2006 root <root@wanda.metarpa> - 1.12-1
- aggiornamento codici per DB-all.e 2.3

* Tue May 16 2006 root <root@wanda.metarpa> - 1.11-1
- aumentata dimesione vettori per lettura grib e corretta gestione osservati su box

* Wed May 10 2006 root <root@wanda.metarpa> - 1.10-1
- corretto baco lettura osservati nel calcolo degli score probabilistici

* Mon May  8 2006 root <root@wanda.metarpa> - 1.9-1
- corretta gestione codice per lsm e orog

* Fri May  5 2006 root <root@wanda.metarpa> - 1.8-1
- corretto baco del baco

* Thu May  4 2006 root <root@wanda.metarpa> - 1.7-1
- correzione baco

* Wed May  3 2006 root <root@wanda.metarpa> - 1.6-1
- corretto baco

* Wed May  3 2006 root <root@wanda.metarpa> - 1.5-1
- aggiunta ottimizzazione lettura e pulizia

* Tue Apr 18 2006 root <root@wanda.metarpa> - 1.3-1
- corretto i sorgenti e aggiunto caricamento osservati cosmo

* Wed Mar  1 2006 root <root@wanda.metarpa> - 1.1-1
- corretto bug in estrazione

* Tue Feb 28 2006 root <root@strip.metarpa> - 
- Initial build.

