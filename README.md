# mondrian

A solver for the puzzle game https://mondrianblocks.com/

## 1) Was wurde gemacht?
Wir haben für das Puzzle-Spiel Mondrian Blocks ein Lösungs-Programm entwickelt.

Bei Mondrian Blocks gibt es ein quadratisches, 8x8 Felder großes Spielfeld. Außerdem gibt es einige Blöcke, die das Spielfeld zusammen vollständig ausfüllen. Am Start des Spiels sind einige Blöcke jeweils bereits auf dem Spielfeld platziert, und die restlichen Blöcke müssen nun so angeordnet werden, dass alle auf dem Spielfeld Platz finden. <p>
Mit unserem Programm kann nun diese Ausgangslage per Konsoleneingabe eingegeben, und alle Lösungen für diese angezeigt werden. 
Der Lösungsalgorithmus basiert darauf, einen Block nach dem anderen an allen noch freien Positionen des Spielfeldes zu platzieren. Jede dieser Platzierungen wird als Kopie des Spielfelds gespeichert und als Ausgangslage für den nächsten Berechnungsschritt genutzt. In den meisten Fällen bleibt irgendwann kein Platz mehr für den nächsten Block, und dieser Lösungsansatz wird verworfen. Passiert dies nie, handelt es sich um eine valide Lösung, die schließlich zurück gegeben wird.
Im einem Berechnungsschritt wird also der ein Block auf jedes freie Feld des Spielbretts platziert. Für jede Möglichkeit wird  eine Kopie des Spielfelds mit diesem platzierten Block erstellt. Auf jede dieser Kopien wird nun wieder dieser Berechnungsschritt angewandt. Dies wird wiederholt, bis keine Blöcke mehr übrig sind. <p>
Wir haben auch Optimierungsmöglichkeiten für den Algorithmus probiert. Beispiel: Die kleinsten Blöcke zuerst platzieren, um, sollte es auf dem Spielfeld eine 1-breite Lücke, aber keinen so schmalen Block mehr geben, den Versuch direkt zu verwerfen. Es hat sich aber herausgestellt, dass es am effizientesten ist, die größten Blöcke zuerst zu platzieren, um so früh die Anzahl zu prüfender Varianten zu begrenzen.

## 2) Wie ist der Installationsprozess?
Es gibt keine Dependencies, das Programm kann einfach durch Aufruf von main in Game gestartet werden.

## 3) Wie ist die Bedienung?
Wir haben einen Algorithmus entwickelt, der alle möglichen Anordnungen der übrigen Blöcke findet, sodass alle Blöcke auf das Spielfeld passen. Die Positionen der vorgegebenen Blöcke können per Konsoleneingabe festgelegt werden, und anschließend werden alle gefundenen Lösungen angezeigt.
Außerdem haben wir eine weitere, allgemeinere Konsoleneingabe implementiert, bei der die Größe des Spielfelds und die Anzahl, Position und Maße aller Blöcke komplett frei angegeben werden kann. Der zugrunde liegende Lösungsalgorithmus ist hierbei der selbe.


## - Listen - ja/nein
Listen werden in den meisten Funktionen verwendet. Bei der Berechnung aller Lösungsmöglichkeiten werden die verschiedenen Spielbretter, welche alle möglichen Kombinationen darstellen, als Liste gespeichert. 

## - list comprehension - ja/nein
Wird verwendet, etwa in Collision/allPositionsBoard.

## - Funktionen mit pattern matching - ja/nein	
In zahlreichen Funktionen wird Pattern Matching verwendet, etwa in Collision/placeOnBlocksIfLegal, wo es verwendet wird, um eine Liste zu Teilen und Rekursion zu vereinfachen. Auch in Collision/isInBounds, Collision/allOccupiedPositions, und anderen Funktionen wird es verwendet.

## - Funktionen mit guards - ja/nein
Ja, wird etwa in Collision/placePositionsIfLegal  verwendet.

## - Rekursive Funktionen - ja/nein
Ja, Rekursion wird an allen Stellen eingesetzt, wo wiederholte Ausführung nötig ist, etwa in Collision/placeOnPositionsIfLegal.

## - Funktionen höherer Ordnung wie map, filter, fold - ja/nein
Ja, zum Beispiel concatMap in Collision/solveGame und Collision/allOccupiedPositionsBoard.

## - Fehlerbehandlung ggf. mit Either oder Maybe - ja/nein
Nein. Der Algorithmus wurde so entwickelt, dass Fehlerbehandlung nicht notwendig ist. Wenn es keine validen Lösungen für den eingegebenen Spielstand gibt, dann ist das kein Fehler, entsprechend wird eine leere Liste zurück gegeben.

## - Eigene Datentypen - ja/nein
Ja. Eigene Datentypen waren essenziell, um das Programm effizient umzusetzen. Analog zu den realen Spielelementen – Spielbrett und Blöcke – sind in GameElements Datentypen definiert.

## - Ein/Ausgabe mit IO-Monaden - ja/nein
Nein.

## - Modularisierung - ja/nein
Ja. Bereits erwähnt wurde, dass die Datentypen in GameElements ausgelagert wurde. 	Der eigentliche Algorithmus ist so gekapselt in Collision.

## - Übersichtlicher Code (ggf. let / where verwendet) - ja/nein
Damit der Code möglichst leserlich ist, haben wir den Ablauf in möglichst viele Teilfunktionen zerlegt und mit Kommentaren ergänzt. Wo eine weitere Zerlegung nicht möglich war, haben wir where und let verwendet, um den Ablauf innerhalb der Funktion deutlicher zu machen, etwa in Collision/placeOnPositionsIfLegal und Collision/solveSingleBlock.

## - wichtigste Teile des Codes dokumentiert - ja/nein
Ja. Alle Funktionen des Lösungsalgorithmus sind kommentiert, um die Implementierung nachvollziehbarer zu machen.
