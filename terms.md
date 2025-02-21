# drakon terms

* valent point
  * of the first kind
    * visual words containing valent points of first kind:
      * fork
      * switch
      * arrow loop
      * switch loop
      * for loop
      * wait loop
      * timer-driven fork
      * timer-driven switch
      * timer-driven arrow loop
      * timer-driven switch loop
      * timer-driven for loop
      * timer-driven wait loop
  * of second kind: located at the entrance or exit of a skewer block
* connecting line/connection
  * could have a valent point at which it can be broken
* skewer-block
  * atom
    * drakon-letter that is a skewer-block
    * drakon-word that is a skewer-block
    * except
      * headline
      * address
      * begin of for loop
      * end of for loop
    * empty if equivalent to an empty operator (e.g. empty fork is an empty atom)
  * alpha-element
  * beta-element
* empty atoms are allowed for all stages of designing a drakon chart except for the last stage
* compound skewer block - a sequence of atoms
* a set of alpha-elements is obtained from the set of sequences of atoms by multiple application of the insert operation
* liana
* transplantation of a liana
* icon
* axiom-primitive chart
* axiom-silhouette chart
* axiom valent point
* insert a skewer-block operation
  * skewer-block replaces a valent point
  * axiom valent point of type/kind 1
  * drakon-chart valent point of type/kind 1 or 2
* operation: selection of an initial terminator
  * icon 1 (Title) of primitive axiom or silhouette axiom can be replaced by:
    * Cyclic start letter
    * Title with parameters word
    * Cyclic start with parameters word
* operation: deletion of the end icon of a primitive
  * only applies to primitive drakon-charts
* primitive drakon-chart
  * starts as a axiom-primitive chart
  * finite number of the following operations:
    * insertion of a skewer-block
    * transplantation of a liana
    * selection of an initial terminator
    * deletion of the end of a primitive

---

* valent point
  * drakon-chart valent point
    * first kind/type
    * second kind/type
  * axiom valent point
    * first kind/type
    * second kind/type
