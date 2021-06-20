
<!-- README.md is generated from README.Rmd. Please edit that file -->

# «In den Kommentaren zu Roger Köppels YouTube-Sendung finden sich strafbare Äusserungen und Verschwörungstheorien. Eine Übersicht.»

Dieses Repo dient der Dokumentation der Datenanalyse und -beschaffung
zum Blogartikel im Forschungsseminar Politischer Datenjournalismus
FS2021.

## Idee

Dieses Projekt prüft alle YouTube-Kommentare zu Roger Köppels
wochentäglichem Podcast algorithmisch auf beleidigende Inhalte (s.
Hinweise).

Ich versuche u.a. herauszufinden, ob

-   die YouTube-Kommentarspalte zu Weltwoche Daily ein Sammelplatz
    toxischer Wortmeldungen ist,
    -   Anteil Beleidigungen gemäss Algorithmus an allen Kommentaren
    -   absolute Anzahl Beleidigungen gemäss Algorithmus
-   sich offensichtlich strafbare Beleidigungen unter den vom
    Algorithmus identifizierten Kommentaren befinden
    -   manuelle Prüfung eines Samples von Kommentaren
-   allfällige gemäss Algorithmus beleidigende Kommentare einem Muster
    folgen: Gibt es Themen bei denen besonders viele beleidigenden
    Kommentare gefunden werden?
    -   manuelle Prüfung bei Ausschlägen: Bei welchen Episoden und was
        sind deren Inhalte?
    -   (allenfalls: Regression Anzahl toxische Kommentare \~ Präsenz
        eines Topic Models Topics (anhand von Untertiteln von Roger
        Köppels))
-   Köppel mit seinen Inhalten schlechte Laune auslöst
    -   (Kausalität schwierig, aber Einordnung (YouTube als toxischer
        Ort), Supply & Demand)
-   (YouTubes aktuelles System akzeptabel ist)
    -   Einbettung: Löschung nur bei Verstössen gegen
        Community-Richtlinien (Kommentare müssen dazu aber gemeldet
        werden), in Deutschland mit NetzDG.

## Struktur

-   `data`: Aufbereitete Daten
-   `raw`: Rohdaten
-   `src`: Skripte zur Datenanalyse, -beschaffung, -visualisierung und
    Blogbeitrag

**Alle Grafiken, Tabellen et cetera werden durch `08_auswertungen.Rmd`
generiert.**

Achtung: Folgende Rohdaten wurden zum Schutz der Privatsphäre der
Kommentierenden oder aufgrund der Dateigrössenbeschränkungen von GitHub
entfernt:

-   raw/comments/\*
-   data/kommentare.csv
-   raw/kommentare\_sparse.rds
-   data/stm\_models.rds
-   data/stm\_models\_kommentare.rds

## Hinweise

-   Es werden ledigliche alleinstehende YouTube-Kommentare und Antworten
    auf diese verwendet. Antworten auf Antworten werden aus Gründen der
    API-Limitierungen ausgelassen.
-   Die Erkennung von beleidigenden Inhalten geschieht mit dem
    vortrainierten Algorithmus
    [deepset/bert-base-german-cased-hatespeech-GermEval18Coarse](https://huggingface.co/deepset/bert-base-german-cased-hatespeech-GermEval18Coarse),
    der beleidigende Inhalte und Hate Speech erkennt.
-   Das Skript `05_check_for_hatespeech.R` setzt zur Verwendung des
    vortrainierten BERT-Algorithmus ein Python-Environment voraus, für
    das er mein R-Package `{wrappingtransformers}` zurückgreift.
