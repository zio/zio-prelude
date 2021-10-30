---
id: overview_diagrams
title: "Abstraction Diagrams"
---
# Absorption

[![Absorption hierarchy](https://user-images.githubusercontent.com/9019485/139312934-c76ebe2a-98d9-40ab-a5eb-f59af3c1396a.png)][Absorption-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Absorption~A~ <|-- DistributiveAbsorption~A~
  Absorption~A~ <|-- Involution~A~
  Absorption~A~ <|-- Noncontradiction~A~
  Absorption~A~ <|-- ExcludedMiddle~A~
  class Absorption~A~{
    () or(=> A, => A): A
    () and(=> A, => A): A
  }
  class DistributiveAbsorption~A~{
    Boolean
    Set[A]
  }
  class Involution~A~{
    Boolean
    () complement(=> A): A
  }
  class Noncontradiction~A~{
    Boolean
    () complement(=> A): A
    () bottom: A
  }
  class ExcludedMiddle~A~{
    Boolean
    () complement(=> A): A
    () top: A
  }
```

</details>

[Absorption-link]: https://mermaid-js.github.io/mermaid-live-editor/edit/#eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFic29ycHRpb25-QX4gPHwtLSBEaXN0cmlidXRpdmVBYnNvcnB0aW9ufkF-XG4gIEFic29ycHRpb25-QX4gPHwtLSBJbnZvbHV0aW9ufkF-XG4gIEFic29ycHRpb25-QX4gPHwtLSBOb25jb250cmFkaWN0aW9ufkF-XG4gIEFic29ycHRpb25-QX4gPHwtLSBFeGNsdWRlZE1pZGRsZX5BflxuICBjbGFzcyBBYnNvcnB0aW9ufkF-e1xuICAgICgpIG9yKD0-IEEsID0-IEEpOiBBXG4gICAgKCkgYW5kKD0-IEEsID0-IEEpOiBBXG4gIH1cbiAgY2xhc3MgRGlzdHJpYnV0aXZlQWJzb3JwdGlvbn5BfntcbiAgICBCb29sZWFuXG4gICAgU2V0W0FdXG4gIH1cbiAgY2xhc3MgSW52b2x1dGlvbn5BfntcbiAgICBCb29sZWFuXG4gICAgKCkgY29tcGxlbWVudCg9PiBBKTogQVxuICB9XG4gIGNsYXNzIE5vbmNvbnRyYWRpY3Rpb25-QX57XG4gICAgQm9vbGVhblxuICAgICgpIGNvbXBsZW1lbnQoPT4gQSk6IEFcbiAgICAoKSBib3R0b206IEFcbiAgfVxuICBjbGFzcyBFeGNsdWRlZE1pZGRsZX5BfntcbiAgICBCb29sZWFuXG4gICAgKCkgY29tcGxlbWVudCg9PiBBKTogQVxuICAgICgpIHRvcDogQVxuICB9IiwibWVybWFpZCI6IntcbiAgXCJ0aGVtZVwiOiBcImRlZmF1bHRcIlxufSIsInVwZGF0ZUVkaXRvciI6ZmFsc2UsImF1dG9TeW5jIjp0cnVlLCJ1cGRhdGVEaWFncmFtIjpmYWxzZX0

# Associative

[![Associative hierarchy](https://user-images.githubusercontent.com/9019485/95663942-b312cc00-0b43-11eb-984d-b48acdb13738.png)][Associative-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Associative~A~ <|-- Commutative~A~
  Associative~A~ <|-- Idempotent~A~
  Associative~A~ <|-- Identity~A~
  Identity~A~ <|-- Inverse~A~
  class Associative~A~{
    Either[E, A: Associative]
    F[A: Associative]: Derive[_, Associative]
    First[A]
    Last[A]
    NonEmptyChunk[A]
    NonEmptyList[A]
    These[A: Associative, B: Associative]
    ❨T1: Associative, ..., T22: Associative❩
    Validation[E, A: Associative]
    ZNonEmptySet[A, B: Associative]

    () combine(=> A, => A): A
  }
  class Commutative~A~{
    And
    F[A: Commutative]: Derive[_, Commutative]
    Either[E: Commutative, A: Commutative]
    Or
    Map[K, V: Commutative]
    Max[A: Ord]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[A: Ord]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    NonEmptySet[A]
    Option[A: Commutative]
    Prod[Boolean]
    Prod[Byte/Char/Double/Float/Int/Long/Short]
    Set[A]
    Sum[Boolean]
    Sum[Byte/Char/Double/Float/Int/Long/Short]
    These[A: Commutative, B: Commutative]
    ❨T1: Commutative, ..., T22: Commutative❩
    Validation[E, A: Commutative]
    ZSet[A, B: Commutative]
    ZNonEmptySet[A, B: Commutative]

    () commute: Commutative[A]
  }
  class Idempotent~A~{
    And
    F[A: Idempotent]: Derive[_, Idempotent]
    Or
    Map[K, V: Idempotent]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    NonEmptySet[A]
    Option[A: Idempotent]
    Prod[Boolean]
    Set[A]
    Sum[Boolean]
    These[A: Idempotent, B: Idempotent]
    ❨T1: Idempotent, ..., T22: Idempotent❩
    Validation[E, A: Idempotent]
    ZSet[A, B: Idempotent]
    ZNonEmptySet[A, B: Idempotent]

    () combineIdempotent(=> A, => A)(Equal[A]): A
    () idempotent(Equal[A]): Idempotent[A]
  }
  class Identity~A~{
    F[A: Identity]: Derive[_, Identity]
    Chunk[A]
    Either[E, A: Identity]
    List[A]
    Map[K, V: Associative]
    Max[Boolean]
    Max[Byte/Char/Double/Float/Int/Long/Short]
    Min[Boolean]
    Min[Byte/Char/Double/Float/Int/Long/Short]
    Option[A: Associative]
    Prod[Byte/Char/Double/Float/Int/Long/Short]
    String
    ❨T1: Identity, ..., T22: Identity❩
    Validation[E, A: Identity]
    Vector[A]
    ZSet[A, B: Associative]

    () identity: A
  }
  class Inverse~A~{
    And
    F[A: Inverse]: Derive[_, Inverse]
    Or
    Prod[Boolean]
    Set[A]
    Sum[Boolean]
    Sum[Byte/Char/Double/Float/Int/Long/Short]
    ❨T1: Inverse, ..., T22: Inverse❩

    () inverse(=> A, => A): A
  }
```

</details>

[Associative-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gQ29tbXV0YXRpdmV-QX5cbiAgQXNzb2NpYXRpdmV-QX4gPHwtLSBJZGVtcG90ZW50fkF-XG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gSWRlbnRpdHl-QX5cbiAgSWRlbnRpdHl-QX4gPHwtLSBJbnZlcnNlfkF-XG4gIGNsYXNzIEFzc29jaWF0aXZlfkF-e1xuICAgIEVpdGhlcltFLCBBOiBBc3NvY2lhdGl2ZV1cbiAgICBGW0E6IEFzc29jaWF0aXZlXTogRGVyaXZlW18sIEFzc29jaWF0aXZlXVxuICAgIEZpcnN0W0FdXG4gICAgTGFzdFtBXVxuICAgIE5vbkVtcHR5Q2h1bmtbQV1cbiAgICBOb25FbXB0eUxpc3RbQV1cbiAgICBUaGVzZVtBOiBBc3NvY2lhdGl2ZSwgQjogQXNzb2NpYXRpdmVdXG4gICAg4p2oVDE6IEFzc29jaWF0aXZlLCAuLi4sIFQyMjogQXNzb2NpYXRpdmXinalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IEFzc29jaWF0aXZlXVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBBc3NvY2lhdGl2ZV1cblxuICAgICgpIGNvbWJpbmUoPT4gQSwgPT4gQSk6IEFcbiAgfVxuICBjbGFzcyBDb21tdXRhdGl2ZX5BfntcbiAgICBBbmRcbiAgICBGW0E6IENvbW11dGF0aXZlXTogRGVyaXZlW18sIENvbW11dGF0aXZlXVxuICAgIEVpdGhlcltFOiBDb21tdXRhdGl2ZSwgQTogQ29tbXV0YXRpdmVdXG4gICAgT3JcbiAgICBNYXBbSywgVjogQ29tbXV0YXRpdmVdXG4gICAgTWF4W0E6IE9yZF1cbiAgICBNYXhbQm9vbGVhbl1cbiAgICBNYXhbQnl0ZS9DaGFyL0RvdWJsZS9GbG9hdC9JbnQvTG9uZy9TaG9ydF1cbiAgICBNaW5bQTogT3JkXVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgT3B0aW9uW0E6IENvbW11dGF0aXZlXVxuICAgIFByb2RbQm9vbGVhbl1cbiAgICBQcm9kW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgU3VtW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgVGhlc2VbQTogQ29tbXV0YXRpdmUsIEI6IENvbW11dGF0aXZlXVxuICAgIOKdqFQxOiBDb21tdXRhdGl2ZSwgLi4uLCBUMjI6IENvbW11dGF0aXZl4p2pXG4gICAgVmFsaWRhdGlvbltFLCBBOiBDb21tdXRhdGl2ZV1cbiAgICBaU2V0W0EsIEI6IENvbW11dGF0aXZlXVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBDb21tdXRhdGl2ZV1cblxuICAgICgpIGNvbW11dGU6IENvbW11dGF0aXZlW0FdXG4gIH1cbiAgY2xhc3MgSWRlbXBvdGVudH5BfntcbiAgICBBbmRcbiAgICBGW0E6IElkZW1wb3RlbnRdOiBEZXJpdmVbXywgSWRlbXBvdGVudF1cbiAgICBPclxuICAgIE1hcFtLLCBWOiBJZGVtcG90ZW50XVxuICAgIE1heFtCb29sZWFuXVxuICAgIE1heFtCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgT3B0aW9uW0E6IElkZW1wb3RlbnRdXG4gICAgUHJvZFtCb29sZWFuXVxuICAgIFNldFtBXVxuICAgIFN1bVtCb29sZWFuXVxuICAgIFRoZXNlW0E6IElkZW1wb3RlbnQsIEI6IElkZW1wb3RlbnRdXG4gICAg4p2oVDE6IElkZW1wb3RlbnQsIC4uLiwgVDIyOiBJZGVtcG90ZW504p2pXG4gICAgVmFsaWRhdGlvbltFLCBBOiBJZGVtcG90ZW50XVxuICAgIFpTZXRbQSwgQjogSWRlbXBvdGVudF1cbiAgICBaTm9uRW1wdHlTZXRbQSwgQjogSWRlbXBvdGVudF1cblxuICAgICgpIGNvbWJpbmVJZGVtcG90ZW50KD0-IEEsID0-IEEpKEVxdWFsW0FdKTogQVxuICAgICgpIGlkZW1wb3RlbnQoRXF1YWxbQV0pOiBJZGVtcG90ZW50W0FdXG4gIH1cbiAgY2xhc3MgSWRlbnRpdHl-QX57XG4gICAgRltBOiBJZGVudGl0eV06IERlcml2ZVtfLCBJZGVudGl0eV1cbiAgICBDaHVua1tBXVxuICAgIEVpdGhlcltFLCBBOiBJZGVudGl0eV1cbiAgICBMaXN0W0FdXG4gICAgTWFwW0ssIFY6IEFzc29jaWF0aXZlXVxuICAgIE1heFtCb29sZWFuXVxuICAgIE1heFtCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE1pbltCb29sZWFuXVxuICAgIE1pbltCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE9wdGlvbltBOiBBc3NvY2lhdGl2ZV1cbiAgICBQcm9kW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgU3RyaW5nXG4gICAg4p2oVDE6IElkZW50aXR5LCAuLi4sIFQyMjogSWRlbnRpdHninalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IElkZW50aXR5XVxuICAgIFZlY3RvcltBXVxuICAgIFpTZXRbQSwgQjogQXNzb2NpYXRpdmVdXG5cbiAgICAoKSBpZGVudGl0eTogQVxuICB9XG4gIGNsYXNzIEludmVyc2V-QX57XG4gICAgQW5kXG4gICAgRltBOiBJbnZlcnNlXTogRGVyaXZlW18sIEludmVyc2VdXG4gICAgT3JcbiAgICBQcm9kW0Jvb2xlYW5dXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgU3VtW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAg4p2oVDE6IEludmVyc2UsIC4uLiwgVDIyOiBJbnZlcnNl4p2pXG5cbiAgICAoKSBpbnZlcnNlKD0-IEEsID0-IEEpOiBBXG4gIH1cbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19fQ


# AssociativeBoth

[![AssociativeBoth hierarchy](https://user-images.githubusercontent.com/9019485/106826973-47231900-6688-11eb-91e7-929f3b25dca6.png)][AssociativeBoth-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  AssociativeBoth~F<_>~ <|-- CommutativeBoth~F<_>~
  AssociativeBoth~F<_>~ <|-- IdentityBoth~F<_>~
  class AssociativeBoth~F<_>~{
    Fiber[E, +*]
    STM[R, E, +*]

    () both[A,B](=> F[A], => F[B]): F[(A,B)]
  }
  class CommutativeBoth~F<_>~{
    Chunk[+*]
    Exit[E, +*]
    Id[+*]
    List[+*]
    NonEmptyChunk[+*]
    Option[+*]
    AndF[Schedule[R, E, +*]]
    OrF[Schedule[R, E, +*]]
    Vector[+*]
    ZIO[R, E, +*]
    Failure[ZIO[R, E, +*]]
    ZLayer[R, E, +*]
    ZManaged[R, E, +*]
    Failure[ZManaged[R, E, +*]]
    ZSink[R, E, I, I, +*]
    ZStream[R, E, +*]
  }
  class IdentityBoth~F<_>~{
    Either[L, +*]
    Failure[Either[+*, R]]
    Option[+*]
    Future[+*]
    Id[+*]
    List[+*]
    Try[+*]
 
    () any: F[Any]
  }
```

</details>

[AssociativeBoth-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlQm90aH5GPF8-fiA8fC0tIENvbW11dGF0aXZlQm90aH5GPF8-flxuICBBc3NvY2lhdGl2ZUJvdGh-RjxfPn4gPHwtLSBJZGVudGl0eUJvdGh-RjxfPn5cbiAgY2xhc3MgQXNzb2NpYXRpdmVCb3RofkY8Xz5-e1xuICAgIEZpYmVyW0UsICsqXVxuICAgIFNUTVtSLCBFLCArKl1cblxuICAgICgpIGJvdGhbQSxCXSg9PiBGW0FdLCA9PiBGW0JdKTogRlsoQSxCKV1cbiAgfVxuICBjbGFzcyBDb21tdXRhdGl2ZUJvdGh-RjxfPn57XG4gICAgQ2h1bmtbKypdXG4gICAgRXhpdFtFLCArKl1cbiAgICBJZFsrKl1cbiAgICBMaXN0WysqXVxuICAgIE5vbkVtcHR5Q2h1bmtbKypdXG4gICAgT3B0aW9uWysqXVxuICAgIEFuZEZbU2NoZWR1bGVbUiwgRSwgKypdXVxuICAgIE9yRltTY2hlZHVsZVtSLCBFLCArKl1dXG4gICAgVmVjdG9yWysqXVxuICAgIFpJT1tSLCBFLCArKl1cbiAgICBGYWlsdXJlW1pJT1tSLCBFLCArKl1dXG4gICAgWkxheWVyW1IsIEUsICsqXVxuICAgIFpNYW5hZ2VkW1IsIEUsICsqXVxuICAgIEZhaWx1cmVbWk1hbmFnZWRbUiwgRSwgKypdXVxuICAgIFpTaW5rW1IsIEUsIEksIEksICsqXVxuICAgIFpTdHJlYW1bUiwgRSwgKypdXG4gIH1cbiAgY2xhc3MgSWRlbnRpdHlCb3RofkY8Xz5-e1xuICAgIEVpdGhlcltMLCArKl1cbiAgICBGYWlsdXJlW0VpdGhlclsrKiwgUl1dXG4gICAgT3B0aW9uWysqXVxuICAgIEZ1dHVyZVsrKl1cbiAgICBJZFsrKl1cbiAgICBMaXN0WysqXVxuICAgIFRyeVsrKl1cbiBcbiAgICAoKSBhbnk6IEZbQW55XVxuICB9IiwibWVybWFpZCI6e30sInVwZGF0ZUVkaXRvciI6ZmFsc2V9


# AssociativeCompose

[![AssociativeCompose hierarchy](https://user-images.githubusercontent.com/9019485/106813134-e50ae980-6670-11eb-9fca-abde850f87cf.png)][AssociativeCompose-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  AssociativeCompose~=>:[-_, +_]~ <|-- IdentityCompose~=>:[-_, +_]~
  AssociativeCompose~=>:[-_, +_]~ <|-- BothCompose~=>:[-_, +_]~
  AssociativeCompose~=>:[-_, +_]~ <|-- EitherCompose~=>:[-_, +_]~
  BothCompose <|-- ApplicationCompose~=>:[-_, +_]~
  class AssociativeCompose~=>:[-_, +_]~{
    () compose[A, B, C](B =>: C, A =>: B): A =>: C
  }
  class IdentityCompose~=>:[-_, +_]~{
    Function[-*, +*]
    URIO[-*, +*]

    () identity[A]: A =>: A
  }
  class BothCompose~=>:[-_, +_]~{
    URIO[-*, +*] [ :*: = Tuple2 ]
  
    (type) :*:[+_, +_]
    () fromFirst[A]: (A :*: Any) =>: A
    () fromSecond[B]: (Any :*: B) =>: B
    () toBoth[A, B, C](A =>: B)(A =>: C): A =>: (B :*: C)
  }
  class ApplicationCompose~=>:[-_, +_]~{
    Function[-*, +*] [ :*: = Tuple2 ; -->: = Function ]
    
    (type) -->:[-_, +_]
    () application[A, B]: ((A -->: B) :*: A) =>: B
    () curry[A, B, C]((A :*: B) =>: C): A =>: (B -->: C)
    () uncurry[A, B, C](A =>: (B -->: C)): (A :*: B) =>: C
  }
  class EitherCompose~=>:[-_, +_]~{
    Function[-*, +*] [ :+: = Either ]
    URIO[-*, +*] [ :+: = Either ]
    
    (type) :+:[+_, +_]
    () toLeft[A]: A =>: (A :+: Nothing)
    () toRight[B]: B =>: (Nothing :+: B)
    () fromEither[A, B, C](=> A =>: C)(=> B =>: C): (A :+: B) =>: C
  }
```

</details>

[AssociativeCompose-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlQ29tcG9zZX49PjpbLV8sICtfXX4gPHwtLSBJZGVudGl0eUNvbXBvc2V-PT46Wy1fLCArX11-XG4gIEFzc29jaWF0aXZlQ29tcG9zZX49PjpbLV8sICtfXX4gPHwtLSBCb3RoQ29tcG9zZX49PjpbLV8sICtfXX5cbiAgQXNzb2NpYXRpdmVDb21wb3Nlfj0-OlstXywgK19dfiA8fC0tIEVpdGhlckNvbXBvc2V-PT46Wy1fLCArX11-XG4gIEJvdGhDb21wb3NlIDx8LS0gQXBwbGljYXRpb25Db21wb3Nlfj0-OlstXywgK19dflxuICBjbGFzcyBBc3NvY2lhdGl2ZUNvbXBvc2V-PT46Wy1fLCArX11-e1xuICAgICgpIGNvbXBvc2VbQSwgQiwgQ10oQiA9PjogQywgQSA9PjogQik6IEEgPT46IENcbiAgfVxuICBjbGFzcyBJZGVudGl0eUNvbXBvc2V-PT46Wy1fLCArX11-e1xuICAgIEZ1bmN0aW9uWy0qLCArKl1cbiAgICBVUklPWy0qLCArKl1cblxuICAgICgpIGlkZW50aXR5W0FdOiBBID0-OiBBXG4gIH1cbiAgY2xhc3MgQm90aENvbXBvc2V-PT46Wy1fLCArX11-e1xuICAgIFVSSU9bLSosICsqXSBbIDoqOiA9IFR1cGxlMiBdXG4gIFxuICAgICh0eXBlKSA6KjpbK18sICtfXVxuICAgICgpIGZyb21GaXJzdFtBXTogKEEgOio6IEFueSkgPT46IEFcbiAgICAoKSBmcm9tU2Vjb25kW0JdOiAoQW55IDoqOiBCKSA9PjogQlxuICAgICgpIHRvQm90aFtBLCBCLCBDXShBID0-OiBCKShBID0-OiBDKTogQSA9PjogKEIgOio6IEMpXG4gIH1cbiAgY2xhc3MgQXBwbGljYXRpb25Db21wb3Nlfj0-OlstXywgK19dfntcbiAgICBGdW5jdGlvblstKiwgKypdIFsgOio6ID0gVHVwbGUyIDsgLS0-OiA9IEZ1bmN0aW9uIF1cbiAgICBcbiAgICAodHlwZSkgLS0-OlstXywgK19dXG4gICAgKCkgYXBwbGljYXRpb25bQSwgQl06ICgoQSAtLT46IEIpIDoqOiBBKSA9PjogQlxuICAgICgpIGN1cnJ5W0EsIEIsIENdKChBIDoqOiBCKSA9PjogQyk6IEEgPT46IChCIC0tPjogQylcbiAgICAoKSB1bmN1cnJ5W0EsIEIsIENdKEEgPT46IChCIC0tPjogQykpOiAoQSA6KjogQikgPT46IENcbiAgfVxuICBjbGFzcyBFaXRoZXJDb21wb3Nlfj0-OlstXywgK19dfntcbiAgICBGdW5jdGlvblstKiwgKypdIFsgOis6ID0gRWl0aGVyIF1cbiAgICBVUklPWy0qLCArKl0gWyA6KzogPSBFaXRoZXIgXVxuICAgIFxuICAgICh0eXBlKSA6KzpbK18sICtfXVxuICAgICgpIHRvTGVmdFtBXTogQSA9PjogKEEgOis6IE5vdGhpbmcpXG4gICAgKCkgdG9SaWdodFtCXTogQiA9PjogKE5vdGhpbmcgOis6IEIpXG4gICAgKCkgZnJvbUVpdGhlcltBLCBCLCBDXSg9PiBBID0-OiBDKSg9PiBCID0-OiBDKTogKEEgOis6IEIpID0-OiBDXG4gIH0iLCJtZXJtYWlkIjp7fSwidXBkYXRlRWRpdG9yIjpmYWxzZX0


# AssociativeEither

[![AssociativeCompose hierarchy](https://user-images.githubusercontent.com/10689197/99921919-9699b000-2cfb-11eb-9323-4210eb72d2f1.png)][AssociativeEither-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  AssociativeEither~F<_>~ <|-- CommutativeEither~F<_>~
  AssociativeEither~F<_>~ <|-- IdentityEither~F<_>~
  class AssociativeEither~F<_>~{
    Either[L, +*]
    Exit[E, +*]
    Fiber[E, +*]
    Schedule[R, E, +*]
    Try[+*]
    ZLayer[R, E, +*]
    ZManaged[R, E, +*]
    
    () either[A, B](=> F[A], => F[B]): F[Either[A, B]]
  }
  class CommutativeEither~F<_>~{
    Future[+*]
    ZIO[R, E, +*] 
    ZSink[R, E, I, L, +*]
    ZStream[R, E, +*]
    Equal[-*]
  }
  class IdentityEither~F<_>~{
    Option[+*]
    Equal[-*]
    Hash[-*]
    Ord[-*]

    () none: F[Nothing]
  }
```

</details>


[AssociativeEither-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlRWl0aGVyfkY8Xz5-IDx8LS0gQ29tbXV0YXRpdmVFaXRoZXJ-RjxfPn5cbiAgQXNzb2NpYXRpdmVFaXRoZXJ-RjxfPn4gPHwtLSBJZGVudGl0eUVpdGhlcn5GPF8-flxuICBjbGFzcyBBc3NvY2lhdGl2ZUVpdGhlcn5GPF8-fntcbiAgICBFaXRoZXJbTCwgKypdXG4gICAgRXhpdFtFLCArKl1cbiAgICBGaWJlcltFLCArKl1cbiAgICBTY2hlZHVsZVtSLCBFLCArKl1cbiAgICBUcnlbKypdXG4gICAgWkxheWVyW1IsIEUsICsqXVxuICAgIFpNYW5hZ2VkW1IsIEUsICsqXVxuICAgIFxuICAgICgpIGVpdGhlcltBLCBCXSg9PiBGW0FdLCA9PiBGW0JdKTogRltFaXRoZXJbQSwgQl1dXG4gIH1cbiAgY2xhc3MgQ29tbXV0YXRpdmVFaXRoZXJ-RjxfPn57XG4gICAgRnV0dXJlWysqXVxuICAgIFpJT1tSLCBFLCArKl0gXG4gICAgWlNpbmtbUiwgRSwgSSwgTCwgKypdXG4gICAgWlN0cmVhbVtSLCBFLCArKl1cbiAgICBFcXVhbFstKl1cbiAgfVxuICBjbGFzcyBJZGVudGl0eUVpdGhlcn5GPF8-fntcbiAgICBPcHRpb25bKypdXG4gICAgRXF1YWxbLSpdXG4gICAgSGFzaFstKl1cbiAgICBPcmRbLSpdXG5cbiAgICAoKSBub25lOiBGW05vdGhpbmddXG4gIH0iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCIsInRoZW1lVmFyaWFibGVzIjp7ImJhY2tncm91bmQiOiJ3aGl0ZSIsInByaW1hcnlDb2xvciI6IiNFQ0VDRkYiLCJzZWNvbmRhcnlDb2xvciI6IiNmZmZmZGUiLCJ0ZXJ0aWFyeUNvbG9yIjoiaHNsKDgwLCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5Qm9yZGVyQ29sb3IiOiJoc2woMjQwLCA2MCUsIDg2LjI3NDUwOTgwMzklKSIsInNlY29uZGFyeUJvcmRlckNvbG9yIjoiaHNsKDYwLCA2MCUsIDgzLjUyOTQxMTc2NDclKSIsInRlcnRpYXJ5Qm9yZGVyQ29sb3IiOiJoc2woODAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwicHJpbWFyeVRleHRDb2xvciI6IiMxMzEzMDAiLCJzZWNvbmRhcnlUZXh0Q29sb3IiOiIjMDAwMDIxIiwidGVydGlhcnlUZXh0Q29sb3IiOiJyZ2IoOS41MDAwMDAwMDAxLCA5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSkiLCJsaW5lQ29sb3IiOiIjMzMzMzMzIiwidGV4dENvbG9yIjoiIzMzMyIsIm1haW5Ca2ciOiIjRUNFQ0ZGIiwic2Vjb25kQmtnIjoiI2ZmZmZkZSIsImJvcmRlcjEiOiIjOTM3MERCIiwiYm9yZGVyMiI6IiNhYWFhMzMiLCJhcnJvd2hlYWRDb2xvciI6IiMzMzMzMzMiLCJmb250RmFtaWx5IjoiXCJ0cmVidWNoZXQgbXNcIiwgdmVyZGFuYSwgYXJpYWwiLCJmb250U2l6ZSI6IjE2cHgiLCJsYWJlbEJhY2tncm91bmQiOiIjZThlOGU4Iiwibm9kZUJrZyI6IiNFQ0VDRkYiLCJub2RlQm9yZGVyIjoiIzkzNzBEQiIsImNsdXN0ZXJCa2ciOiIjZmZmZmRlIiwiY2x1c3RlckJvcmRlciI6IiNhYWFhMzMiLCJkZWZhdWx0TGlua0NvbG9yIjoiIzMzMzMzMyIsInRpdGxlQ29sb3IiOiIjMzMzIiwiZWRnZUxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJhY3RvckJvcmRlciI6ImhzbCgyNTkuNjI2MTY4MjI0MywgNTkuNzc2NTM2MzEyOCUsIDg3LjkwMTk2MDc4NDMlKSIsImFjdG9yQmtnIjoiI0VDRUNGRiIsImFjdG9yVGV4dENvbG9yIjoiYmxhY2siLCJhY3RvckxpbmVDb2xvciI6ImdyZXkiLCJzaWduYWxDb2xvciI6IiMzMzMiLCJzaWduYWxUZXh0Q29sb3IiOiIjMzMzIiwibGFiZWxCb3hCa2dDb2xvciI6IiNFQ0VDRkYiLCJsYWJlbEJveEJvcmRlckNvbG9yIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwibGFiZWxUZXh0Q29sb3IiOiJibGFjayIsImxvb3BUZXh0Q29sb3IiOiJibGFjayIsIm5vdGVCb3JkZXJDb2xvciI6IiNhYWFhMzMiLCJub3RlQmtnQ29sb3IiOiIjZmZmNWFkIiwibm90ZVRleHRDb2xvciI6ImJsYWNrIiwiYWN0aXZhdGlvbkJvcmRlckNvbG9yIjoiIzY2NiIsImFjdGl2YXRpb25Ca2dDb2xvciI6IiNmNGY0ZjQiLCJzZXF1ZW5jZU51bWJlckNvbG9yIjoid2hpdGUiLCJzZWN0aW9uQmtnQ29sb3IiOiJyZ2JhKDEwMiwgMTAyLCAyNTUsIDAuNDkpIiwiYWx0U2VjdGlvbkJrZ0NvbG9yIjoid2hpdGUiLCJzZWN0aW9uQmtnQ29sb3IyIjoiI2ZmZjQwMCIsInRhc2tCb3JkZXJDb2xvciI6IiM1MzRmYmMiLCJ0YXNrQmtnQ29sb3IiOiIjOGE5MGRkIiwidGFza1RleHRMaWdodENvbG9yIjoid2hpdGUiLCJ0YXNrVGV4dENvbG9yIjoid2hpdGUiLCJ0YXNrVGV4dERhcmtDb2xvciI6ImJsYWNrIiwidGFza1RleHRPdXRzaWRlQ29sb3IiOiJibGFjayIsInRhc2tUZXh0Q2xpY2thYmxlQ29sb3IiOiIjMDAzMTYzIiwiYWN0aXZlVGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsImFjdGl2ZVRhc2tCa2dDb2xvciI6IiNiZmM3ZmYiLCJncmlkQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JrZ0NvbG9yIjoibGlnaHRncmV5IiwiZG9uZVRhc2tCb3JkZXJDb2xvciI6ImdyZXkiLCJjcml0Qm9yZGVyQ29sb3IiOiIjZmY4ODg4IiwiY3JpdEJrZ0NvbG9yIjoicmVkIiwidG9kYXlMaW5lQ29sb3IiOiJyZWQiLCJsYWJlbENvbG9yIjoiYmxhY2siLCJlcnJvckJrZ0NvbG9yIjoiIzU1MjIyMiIsImVycm9yVGV4dENvbG9yIjoiIzU1MjIyMiIsImNsYXNzVGV4dCI6IiMxMzEzMDAiLCJmaWxsVHlwZTAiOiIjRUNFQ0ZGIiwiZmlsbFR5cGUxIjoiI2ZmZmZkZSIsImZpbGxUeXBlMiI6ImhzbCgzMDQsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsImZpbGxUeXBlMyI6ImhzbCgxMjQsIDEwMCUsIDkzLjUyOTQxMTc2NDclKSIsImZpbGxUeXBlNCI6ImhzbCgxNzYsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsImZpbGxUeXBlNSI6ImhzbCgtNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU2IjoiaHNsKDgsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsImZpbGxUeXBlNyI6ImhzbCgxODgsIDEwMCUsIDkzLjUyOTQxMTc2NDclKSJ9fSwidXBkYXRlRWRpdG9yIjpmYWxzZX0


# AssociativeFlatten

[![AssociativeFlatten hierarchy](https://user-images.githubusercontent.com/9019485/104860805-3f940e00-592d-11eb-94dc-d139098ef86d.png)][AssociativeFlatten-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  AssociativeFlatten~F<+_>~ <|-- IdentityFlatten~F<_>~
  class AssociativeFlatten~F<+_>~{
    Map[K, +*]
    
    () flatten[A](F[F[A]]): F[A]
  }
  class IdentityFlatten~F<+_>~{
    Cause[+*]
    Chunk[+*]
    Either[L, +*]
    Exit[E, +*]
    Future[+*]
    Id[+*]
    List[+*]
    NonEmptyChunk[+*]
    Option[+*]
    Try[+*]
    Vector[+*]
    ZIO[R, E, +*]
    ZManaged[R, E, +*]
    ZStream[R, E, +*]

    () any: F[Any]
  }
```

</details>

[AssociativeFlatten-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlRmxhdHRlbn5GPCtfPn4gPHwtLSBJZGVudGl0eUZsYXR0ZW5-RjxfPn5cbiAgY2xhc3MgQXNzb2NpYXRpdmVGbGF0dGVufkY8K18-fntcbiAgICBNYXBbSywgKypdXG4gICAgXG4gICAgKCkgZmxhdHRlbltBXShGW0ZbQV1dKTogRltBXVxuICB9XG4gIGNsYXNzIElkZW50aXR5RmxhdHRlbn5GPCtfPn57XG4gICAgQ2F1c2VbKypdXG4gICAgQ2h1bmtbKypdXG4gICAgRWl0aGVyW0wsICsqXVxuICAgIEV4aXRbRSwgKypdXG4gICAgRnV0dXJlWysqXVxuICAgIElkWysqXVxuICAgIExpc3RbKypdXG4gICAgTm9uRW1wdHlDaHVua1srKl1cbiAgICBPcHRpb25bKypdXG4gICAgVHJ5WysqXVxuICAgIFZlY3RvclsrKl1cbiAgICBaSU9bUiwgRSwgKypdXG4gICAgWk1hbmFnZWRbUiwgRSwgKypdXG4gICAgWlN0cmVhbVtSLCBFLCArKl1cblxuICAgICgpIGFueTogRltBbnldXG4gIH0iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ


# Equal

[![Equal hierarchy](https://user-images.githubusercontent.com/9019485/106971398-22de3f80-674f-11eb-99b4-01c2ad982eff.png)][Equal-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Equal~-A~ <|-- Hash~-A~
  Equal~-A~ <|-- PartialOrd~-A~
  PartialOrd~-A~ <|-- Ord~-A~
  class Equal~A~{
    Chunk[A: Equal]
    Either[A: Equal, B: Equal]
    Exit[E: Equal, A: Equal]
    F[A: Equal]: DeriveEqual[_]
    List[A: Equal]
    NonEmptyChunk[A: Equal]
    NonEmptyList[A: Equal]
    Option[A: Equal]
    ParSeq[A: Equal]
    These[A: Equal, B: Equal]
    Try[A: Equal]
    ❨T1: Equal, ..., T22: Equal❩
    Validation[E, A: Equal]
    Vector[A: Equal]
    ZNonEmptySet[A, B: Equal]
    ZSet[A, B: Equal]

    () both[B](=> Equal[B]): Equal[(A, B)]
    () bothWith[B, C](=> Equal[B])(C => (A, B)): Equal[C]
    () contramap[B](B => A): Equal[B]
    () either[B](=> Equal[B]): Equal[Either[A, B]]
    () eitherWith[B, C](=> Equal[B])(C => Either[A, B]): Equal[C]
    () equal(A, A): Boolean
    () notEqual(A, A): Boolean
    () toScala[A1 <: A]: scala.math.Equiv[A1]
  }
  class Hash~-A~ {
    Boolean
    Byte
    Cause[A]
    Char
    Chunk[A: Hash]
    Class[_]
    Double
    Either[A: Hash, B: Hash]
    F[A: Hash]: Derive[_, Hash]
    Fiber.Id
    Float
    Int
    List[A: Hash]
    Long
    Map[A, B: Hash]
    NonEmptyChunk[A: Hash]
    NonEmptyList[A: Hash]
    NonEmptySet[A]
    Nothing
    Option[A: Hash]
    Ordering
    ParMap[A, B: Hash]
    ParSeq[A: Hash]
    ParSet[A]
    PartialOrdering
    Set[A]
    Short
    String
    These[A: Hash, B: Hash]
    ❨T1: Hash, ..., T22: Hash❩
    Unit
    Validation[E: Hash, A: Hash]
    Vector[A: Hash]
    ZNonEmptySet[A, B: Hash]
    ZSet[A, B: Hash]
    ZTrace

    () both[B](Hash[B]): Hash[(A, B)]
    () bothWith[B, C](Hash[B])(C => (A, B)): Hash[C]
    () contramap[B](B => A): Hash[B]
    () either[B](Hash[B]): Hash[Either[A, B]]
    () eitherWith[B, C](Hash[B])(C => Either[A, B]): Hash[C]
    () hash(A): Int
  }
  class PartialOrd~-A~ {
    Chunk[A: PartialOrd]
    Either[A: PartialOrd, B: PartialOrd]
    F[A: PartialOrd]: Derive[_, PartialOrd]
    List[A: PartialOrd]
    Map[A, B: Equal]
    NonEmptyChunk[A: PartialOrd]
    NonEmptyList[A: PartialOrd]
    NonEmptySet[A]
    Option[A: PartialOrd]
    ParMap[A, B: Equal]
    ParSeq[A: PartialOrd]
    ParSet[A]
    PartialOrdering
    Set[A]
    ❨T1: PartialOrd, ..., T22: PartialOrd❩
    Vector[A: PartialOrd]
    ZNonEmptySet[A, B: PartialOrd]
    ZSet[A, B: PartialOrd]

    () both[B](=> PartialOrd[B]): PartialOrd[(A, B)]
    () bothWith[B, C](=> PartialOrd[B])(C => (A, B)): PartialOrd[C]
    () compare(A, A): PartialOrdering
    () contramap[B](B => A): PartialOrd[B]
    () either[B](=> PartialOrd[B]): PartialOrd[Either[A, B]]
    () eitherWith[B, C](=> PartialOrd[B])(C => Either[A, B]): PartialOrd[C]
    () mapPartialOrdering(PartialOrdering => PartialOrdering): PartialOrd[A]

  }
  class Ord~-A~ {
    Boolean
    Byte
    Char
    Chunk[A: Ord]
    Double
    Either[A: Ord, B: Ord]
    F[A: Ord]: Derive[_, Ord]
    Fiber.Id
    Float
    Int
    List[A: Ord]
    Long
    NonEmptyChunk[A: Ord]
    NonEmptyList[A: Ord]
    Nothing
    Option[A: Ord]
    ParSeq[A: Ord]
    Ordering
    Short
    String
    ❨T1: Ord, ..., T22: Ord❩
    Unit
    Vector[A: Ord]

    () both[B](=> Ord[B]): Ord[(A, B)]
    () bothWith[B, C](=> Ord[B])(C => (A, B)): Ord[C]
    () compare(A, A): Ordering
    () contramap[B](B => A): Ord[B]
    () either[B](=> Ord[B]): Ord[Either[A, B]]
    () eitherWith[B, C](=> Ord[B])(C => Either[A, B]): Ord[C]
    () mapOrdering(Ordering => Ordering): Ord[A]
    () reverse: Ord[A]
    () toScala[A1 <: A]: scala.math.Ordering[A1]
  }
```
</details>

[Equal-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEVxdWFsfi1BfiA8fC0tIEhhc2h-LUF-XG4gIEVxdWFsfi1BfiA8fC0tIFBhcnRpYWxPcmR-LUF-XG4gIFBhcnRpYWxPcmR-LUF-IDx8LS0gT3Jkfi1BflxuICBjbGFzcyBFcXVhbH5BfntcbiAgICBDaHVua1tBOiBFcXVhbF1cbiAgICBFaXRoZXJbQTogRXF1YWwsIEI6IEVxdWFsXVxuICAgIEV4aXRbRTogRXF1YWwsIEE6IEVxdWFsXVxuICAgIEZbQTogRXF1YWxdOiBEZXJpdmVFcXVhbFtfXVxuICAgIExpc3RbQTogRXF1YWxdXG4gICAgTm9uRW1wdHlDaHVua1tBOiBFcXVhbF1cbiAgICBOb25FbXB0eUxpc3RbQTogRXF1YWxdXG4gICAgT3B0aW9uW0E6IEVxdWFsXVxuICAgIFBhclNlcVtBOiBFcXVhbF1cbiAgICBUaGVzZVtBOiBFcXVhbCwgQjogRXF1YWxdXG4gICAgVHJ5W0E6IEVxdWFsXVxuICAgIOKdqFQxOiBFcXVhbCwgLi4uLCBUMjI6IEVxdWFs4p2pXG4gICAgVmFsaWRhdGlvbltFLCBBOiBFcXVhbF1cbiAgICBWZWN0b3JbQTogRXF1YWxdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEVxdWFsXVxuICAgIFpTZXRbQSwgQjogRXF1YWxdXG5cbiAgICAoKSBib3RoW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbKEEsIEIpXVxuICAgICgpIGJvdGhXaXRoW0IsIENdKD0-IEVxdWFsW0JdKShDID0-IChBLCBCKSk6IEVxdWFsW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEVxdWFsW0JdXG4gICAgKCkgZWl0aGVyW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gRXF1YWxbQl0pKEMgPT4gRWl0aGVyW0EsIEJdKTogRXF1YWxbQ11cbiAgICAoKSBlcXVhbChBLCBBKTogQm9vbGVhblxuICAgICgpIG5vdEVxdWFsKEEsIEEpOiBCb29sZWFuXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5FcXVpdltBMV1cbiAgfVxuICBjbGFzcyBIYXNofi1BfiB7XG4gICAgQm9vbGVhblxuICAgIEJ5dGVcbiAgICBDYXVzZVtBXVxuICAgIENoYXJcbiAgICBDaHVua1tBOiBIYXNoXVxuICAgIENsYXNzW19dXG4gICAgRG91YmxlXG4gICAgRWl0aGVyW0E6IEhhc2gsIEI6IEhhc2hdXG4gICAgRltBOiBIYXNoXTogRGVyaXZlW18sIEhhc2hdXG4gICAgRmliZXIuSWRcbiAgICBGbG9hdFxuICAgIEludFxuICAgIExpc3RbQTogSGFzaF1cbiAgICBMb25nXG4gICAgTWFwW0EsIEI6IEhhc2hdXG4gICAgTm9uRW1wdHlDaHVua1tBOiBIYXNoXVxuICAgIE5vbkVtcHR5TGlzdFtBOiBIYXNoXVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgTm90aGluZ1xuICAgIE9wdGlvbltBOiBIYXNoXVxuICAgIE9yZGVyaW5nXG4gICAgUGFyTWFwW0EsIEI6IEhhc2hdXG4gICAgUGFyU2VxW0E6IEhhc2hdXG4gICAgUGFyU2V0W0FdXG4gICAgUGFydGlhbE9yZGVyaW5nXG4gICAgU2V0W0FdXG4gICAgU2hvcnRcbiAgICBTdHJpbmdcbiAgICBUaGVzZVtBOiBIYXNoLCBCOiBIYXNoXVxuICAgIOKdqFQxOiBIYXNoLCAuLi4sIFQyMjogSGFzaOKdqVxuICAgIFVuaXRcbiAgICBWYWxpZGF0aW9uW0U6IEhhc2gsIEE6IEhhc2hdXG4gICAgVmVjdG9yW0E6IEhhc2hdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEhhc2hdXG4gICAgWlNldFtBLCBCOiBIYXNoXVxuICAgIFpUcmFjZVxuXG4gICAgKCkgYm90aFtCXShIYXNoW0JdKTogSGFzaFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiAoQSwgQikpOiBIYXNoW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEhhc2hbQl1cbiAgICAoKSBlaXRoZXJbQl0oSGFzaFtCXSk6IEhhc2hbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBIYXNoW0NdXG4gICAgKCkgaGFzaChBKTogSW50XG4gIH1cbiAgY2xhc3MgUGFydGlhbE9yZH4tQX4ge1xuICAgIENodW5rW0E6IFBhcnRpYWxPcmRdXG4gICAgRWl0aGVyW0E6IFBhcnRpYWxPcmQsIEI6IFBhcnRpYWxPcmRdXG4gICAgRltBOiBQYXJ0aWFsT3JkXTogRGVyaXZlW18sIFBhcnRpYWxPcmRdXG4gICAgTGlzdFtBOiBQYXJ0aWFsT3JkXVxuICAgIE1hcFtBLCBCOiBFcXVhbF1cbiAgICBOb25FbXB0eUNodW5rW0E6IFBhcnRpYWxPcmRdXG4gICAgTm9uRW1wdHlMaXN0W0E6IFBhcnRpYWxPcmRdXG4gICAgTm9uRW1wdHlTZXRbQV1cbiAgICBPcHRpb25bQTogUGFydGlhbE9yZF1cbiAgICBQYXJNYXBbQSwgQjogRXF1YWxdXG4gICAgUGFyU2VxW0E6IFBhcnRpYWxPcmRdXG4gICAgUGFyU2V0W0FdXG4gICAgUGFydGlhbE9yZGVyaW5nXG4gICAgU2V0W0FdXG4gICAg4p2oVDE6IFBhcnRpYWxPcmQsIC4uLiwgVDIyOiBQYXJ0aWFsT3Jk4p2pXG4gICAgVmVjdG9yW0E6IFBhcnRpYWxPcmRdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IFBhcnRpYWxPcmRdXG4gICAgWlNldFtBLCBCOiBQYXJ0aWFsT3JkXVxuXG4gICAgKCkgYm90aFtCXSg9PiBQYXJ0aWFsT3JkW0JdKTogUGFydGlhbE9yZFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gUGFydGlhbE9yZFtCXSkoQyA9PiAoQSwgQikpOiBQYXJ0aWFsT3JkW0NdXG4gICAgKCkgY29tcGFyZShBLCBBKTogUGFydGlhbE9yZGVyaW5nXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IFBhcnRpYWxPcmRbQl1cbiAgICAoKSBlaXRoZXJbQl0oPT4gUGFydGlhbE9yZFtCXSk6IFBhcnRpYWxPcmRbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gUGFydGlhbE9yZFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBQYXJ0aWFsT3JkW0NdXG4gICAgKCkgbWFwUGFydGlhbE9yZGVyaW5nKFBhcnRpYWxPcmRlcmluZyA9PiBQYXJ0aWFsT3JkZXJpbmcpOiBQYXJ0aWFsT3JkW0FdXG5cbiAgfVxuICBjbGFzcyBPcmR-LUF-IHtcbiAgICBCb29sZWFuXG4gICAgQnl0ZVxuICAgIENoYXJcbiAgICBDaHVua1tBOiBPcmRdXG4gICAgRG91YmxlXG4gICAgRWl0aGVyW0E6IE9yZCwgQjogT3JkXVxuICAgIEZbQTogT3JkXTogRGVyaXZlW18sIE9yZF1cbiAgICBGaWJlci5JZFxuICAgIEZsb2F0XG4gICAgSW50XG4gICAgTGlzdFtBOiBPcmRdXG4gICAgTG9uZ1xuICAgIE5vbkVtcHR5Q2h1bmtbQTogT3JkXVxuICAgIE5vbkVtcHR5TGlzdFtBOiBPcmRdXG4gICAgTm90aGluZ1xuICAgIE9wdGlvbltBOiBPcmRdXG4gICAgUGFyU2VxW0E6IE9yZF1cbiAgICBPcmRlcmluZ1xuICAgIFNob3J0XG4gICAgU3RyaW5nXG4gICAg4p2oVDE6IE9yZCwgLi4uLCBUMjI6IE9yZOKdqVxuICAgIFVuaXRcbiAgICBWZWN0b3JbQTogT3JkXVxuXG4gICAgKCkgYm90aFtCXSg9PiBPcmRbQl0pOiBPcmRbKEEsIEIpXVxuICAgICgpIGJvdGhXaXRoW0IsIENdKD0-IE9yZFtCXSkoQyA9PiAoQSwgQikpOiBPcmRbQ11cbiAgICAoKSBjb21wYXJlKEEsIEEpOiBPcmRlcmluZ1xuICAgICgpIGNvbnRyYW1hcFtCXShCID0-IEEpOiBPcmRbQl1cbiAgICAoKSBlaXRoZXJbQl0oPT4gT3JkW0JdKTogT3JkW0VpdGhlcltBLCBCXV1cbiAgICAoKSBlaXRoZXJXaXRoW0IsIENdKD0-IE9yZFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBPcmRbQ11cbiAgICAoKSBtYXBPcmRlcmluZyhPcmRlcmluZyA9PiBPcmRlcmluZyk6IE9yZFtBXVxuICAgICgpIHJldmVyc2U6IE9yZFtBXVxuICAgICgpIHRvU2NhbGFbQTEgPDogQV06IHNjYWxhLm1hdGguT3JkZXJpbmdbQTFdXG4gIH0iLCJtZXJtYWlkIjp7fSwidXBkYXRlRWRpdG9yIjpmYWxzZX0

