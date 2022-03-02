Ein R-Package zur Analyse Schweizer Online-Medien
================
Samuel Meier,
12.04.2022

## Installation

``` r
# install.packages("devtools")
devtools::install_github("samumei/chnews")
```

``` r
library(chnews)
```

Für die Installation wird das Package `devtools` benötigt. Alternativ
kann das Paket auch als ZIP-Datei heruntergeladen werden und dann lokal
installiert werden.

## Funktionen für Web-Scraping

Mit `chnews` stehen eine Vielzahl Funktionen zur Verfügung, welche
Artikel der NZZ, WOZ und von SRF auslesen und herunterladen können.

### Einmaliges Herunterladen

Die Artikel lassen sich durch einmaliges aufrufen der folgenden Funktion
herunterladen.

``` r
# für SRF
srf_articles <- get_srf_articles()

# für WOZ
woz_articles <- get_woz_articles()

# für NZZ
nzz_articles <- get_nzz_articles()
```

Dabei werden die neutsten Artikel der jeweiligen Medien heruntergeladen.
Beim SRF sind das alle Artikel, die unter
[srf.ch/news/das-neueste](https://www.srf.ch/news/das-neueste) zu finden
sind. Bei der WOZ werden die neusten Artikel aus den Ressorts
[Schweiz](https://www.woz.ch/t/schweiz),
[Wirtschaft](https://www.woz.ch/t/wirtschaft),
[International](https://www.woz.ch/t/international) und
[Kultur/Wissen](https://www.woz.ch/t/kultur-wissen) heruntergeladen. Die
neusten Artikel der NZZ werden unter dem link
[nzz.ch/neueste-artikel](https://www.nzz.ch/neueste-artikel) aufgerufen.
Dabei lassen sich mit dem Parameter `n` die Anzahl Artikel die
heruntergeladen werden sollen bestimmen, beispielsweise für die neusten
fünfzig Artikel wird die Funktion mit `get_nzz_articles(n=50)`
aufgerufen. Diesen Parameter gibt es in den Funktion für SRF und WOZ
nicht. In jenen lässt sich hingegen ein Dataframe mit bereits
heruntergeladenen Artikel als Parameter in die Funktion geben, dann
werden die neuen (oder aktualisierten) Artikel an den bestehenden
Dataframe anghängt und zurückgegeben. Damit lassen sich die Artikel
kontinuierlich sammeln ohne die Artikel doppelt abzuspeichern.

``` r
# für SRF
srf_articles <- get_srf_articles()
srf_articles <- get_srf_articles(srf_articles)

# für WOZ
woz_articles <- get_woz_articles()
woz_articles <- get_woz_articles(woz_articles)
```

### Kontinuierlich nach neuen Artikeln suchen

Für WOZ und SRF gibt es zudem die Möglichkeit, in regelmässigen
Abständen die Websiten nach neuen oder aktualisierten Artikeln zu
durchforsten und diese herunterzuladen. Diese Funktionen eignen sich
insbesondere für Untersuchungen über die Häufigkeit, mit der Artikel
nach der ersten Veröffentlichung weiter modifiziert werden. Die Artikel
werden laufend in einem Dataframe abgespeichert.

``` r
# für SRF
srf_articles <- get_srf_articles_continously()

# für WOZ
woz_articles <- get_woz_articles_continously(update_in_minutes = 30, total_updates = 72, save_backup = FALSE)
```

Mit den Parametern `update_in_minutes` und `total_updates` lassen sich
die Häufigkeit und Summe der Updates festlegen. In obenstehenden
Funktionsaufruf für die WOZ würde während zwei Tagen alle 30 Minuten
überprüft, ob es neue oder aktualisierte Arikel gibt. Ist `save_backup`
auf `TRUE` gesetzt (default), dann wird nach jeder Veränderung des
Datensatzes eine CSV-Datei als Backup gespeichert. Damit wird der
Datenverlust bei einem unvorhergesehen Abbruch der Funktion verringert.
Die detaillierte Dokumentation aller exportierten Funktionen lässt sich
mit `?chnews::funktionsname` aufrufen (z.B.
`?chnews::get_woz_articles_continously`).

### Artikel des WOZ-Archivs

Das öffentlich zugänglich Archiv der WOZ ermöglicht es alle Artikel seit
2005 einzusehen. Bei den älteren Artikeln ist der Text teilweise nicht
mehr zugänglich. Titel und Metadaten sind aber bei allen Arikeln
vorhanden. Mit der folgenden Funktion lassen sich alle Artikel seit 2005
herunterladen.

``` r
# Artikel seit 2005 als Liste gespeichert
woz_articles <- get_woz_articles_from_archive(years = seq(2005, 2022, 1))

# Artikel seit zwischen 2013 und 2015 in einem Dataframe gespeichert
woz_articles <- get_woz_articles_from_archive(years = seq(2013, 2015, 1), df = TRUE)
```

Dabei werden die Artikel entweder in einer Liste gespeichert oder, wenn
`df=TRUE` als Dataframe. Ersteres ist deutlich effizienter, die Daten
müssen dann aber noch eingelsen werden für die weitere Analyse. Für den
Dataframe stehen hingegen bereits Funktionen zur Bereinigung und Analyse
der Daten zur Verfügung. Diese werden im Folgenden vorgestellt.

## Funktionen zur Datenaufbereitung

Die heruntergeladenen Daten sind - auch wenn sie als Dataframe
gespeichert werden - noch unaufgeräumt. Es hat redundante Lehrzeichen an
den Rändern, alle Werte sind als Strings abgespeichert und Autor:innen
werden nicht immer einheitlich angegeben bzw. gibt es auch Artikel, die
von mehreren Personen verfasst wurden. Damit die Artikel der drei
Medieninstitutionen mit denselben Funktionen ausgewertet werden können,
müssen sie zuerst entsprechend aufbereinigt werden. Dafür stehen
medienspezifische Funktionen bereit. Diese bereinigen die
heruntergeladenen Daten so, dass sie bereit sind für die Analyse und
einheitlich weiterverwendet werden können.

``` r
# für SRF
srf_articles_raw <- get_srf_articles()
srf_articles_clean <- clean_srf_articles(srf_articles_raw)


# für WOZ
woz_articles_raw <- get_woz_articles()
woz_articles_clean <- clean_woz_articles(woz_articles_raw)
```

Wir sehen, dass der SRF-Datensatz nach der Datenaufbereitung sechs
zusätzliche Spalten aufweist. Diese beinhalten drei Datenspalten, mit
Angaben zum Tag, Monat und Jahr (`day_published`, `month_published`,
`year_published`) der Veröffentlichung eines Artikels. Zudem drei neue
Spalten mit Angaben zur Anzahl Autor:innen eines Artikels (`n_author`)
und deren Namen (`author_1`, `author_2`). In den meisten Fällen hat nur
eine Person einen Artikel geschrieben. In sehr wenigen Fällen waren es
mehr als zwei. Diese Artikel werden nicht berücksichtigt. Dank den
Datenbereinigungsfunktionen, lassen sich die heruntergeladenen Daten mit
nur eine Codezeile bereinigen und sind dann bereit für die
Datenauswertung. Die dafür vorhandenen Funktionen werden im nächsten
Abschnitt vorgestellt. Die Datensätze `srf_articles_raw` und
`srf_articles_clean` in dieser Arbeit sind

## Funktionen für Text-Analyse

Die für die Auswertung der heruntergeladenen Artikel verfügbaren
Funktionen basieren nur auf den Packages `dplyr`, `gplot2` und
`lubridate` und enthalten eine eigene Auswertungslogik. Grundsätzlich
werden die Daten funktionenspezifisch und anhand der Parameter gefiltert
und sortiert und dann mittels `ggplot` dargestellt. Zahlreiche Parameter
ermöglichen eine rasche und vielseitige Auswertung der Daten.

### Artikel-Häufigkeit

Mit der Funktion `plot_article_frequency()` lässt sich die Häufigkeit
der Online-Artikel nach Tag, Monat oder Jahr visualisieren. Im folgenden
Beispiel werden die Artikel

``` r
plot_article_frequency(srf_articles_clean, period = "day", from = "2022-03-01", to = "2022-03-31")
```

### Wichtigkeit der Ressorts

Mit der Funktion `plot_section_frequency()` lassen sich Antworten auf
diese Fragen finden. Dabei kann die absolute oder relative Häufigkeit
der ausgewählten Ressorts verwendet visualisiert werden. Standardmässig
werden die fünf grössten Ressorts visualisert, gemessen am Total der
veröffentlichten Artikel. Es lassen sich aber auch weniger oder mehr
Ressorts anzeigen oder die Ressorts mittels String-Vektor selbst
spezifizieren.

``` r
plot_section_frequency(srf_articles_clean, top = 5, from = "2022-03-01", to = "2022-03-31")
```

### Zentrale Autor:innen

Mit der Funktion `plot_author_frequency()` lässt sich grafisch
darstellen, welchen Autor:innen eine zentrale Bedeutung zukommt, gemesen
an den Anzahl Artikeln, die sie publiziert haben.

``` r
plot_author_frequency(srf_articles_clean, top = 7)
```

### Wer schreibt Artikel gemeinsam?

Obowhl die Mehrheit der Artikel von einer Person verfasst werden, gibt
es zahlreiche Artikel, die von mehreren Autor:innen verfasst werden. Mit
der Funktion `plot_main_collaborators()` wird angezeigt, welche
Journalist:innen am häufigsten mit anderen zusammenarbeiten.

``` r
plot_main_collaborators(srf_articles_clean, top = 6)
```

### Welche Themen werden behandelt?

Die Funktion `plot_topic_frequency()` durchsucht die Texte nach Keywords
und visualisiert, wieviele Artikel diese enthalten. Dabei kann wieder
das Total oder die zeitliche Entwicklung angezeigt werden. Bei der
zeitlichen Entwicklung werden die absolute oder relative Anzahl Artikel
mit einem oder mehreren enthaltenen Wörtern pro Tag, Monat oder Jahr
angezeigt.

``` r
topic <- c("Emmanuel Macron", 
           "Marine Le Pen", 
           "Jean-Luc Mélenchon")

plot_topic_frequency(srf_articles_clean, topic, distinct_only = FALSE)
```

Die Funktion lässt sich äusserst vielfältig anwednen. Beispielsweise um
das Aufkommen oder Verschwinden bestimmter Begriffe zu untersuchen. Oder
um die Dominaz bestimmter Themen, Parteien oder Politiker:innen in
unterschiedlichen Medien zu vergleichen.

## Lizenz

GPL-3

## Autor

[Samuel Meier](https://github.com/samumei)
