---
id: overview_diagrams
title: "Type Class diagrams"
---

# Equal

[![Equal hierarchy][Equal-image]][Equal-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Equal~-A~ <|-- Hash~-A~
  Equal~-A~ <|-- Ord~-A~
  class Equal~A~{
    Chunk[A: Equal]
    Either[A: Equal, B: Equal]
    Exit[E: Equal, A: Equal]
    F[A: Equal]: DeriveEqual[_]
    List[A: Equal]
    Map[A, B: Equal]
    NonEmptyChunk[A: Equal]
    NonEmptyList[A: Equal]
    Option[A: Equal]
    These[A: Equal, B: Equal]
    Try[A: Equal]
    ❨T1: Equal, ..., T22: Equal❩
    Validation[E, A: Equal]
    Vector[A: Equal]
    ZNonEmptySet[A, B: Equal]
    ZSet[A, B: Hash]

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
    Set[A]
    Short
    String
    These[A: Hash, B: Hash]
    Throwable
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

[Equal-image]: https://user-images.githubusercontent.com/9019485/95663624-0afc0380-0b41-11eb-8884-bd36f747a01f.png
[Equal-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEVxdWFsfi1BfiA8fC0tIEhhc2h-LUF-XG4gIEVxdWFsfi1BfiA8fC0tIE9yZH4tQX5cbiAgY2xhc3MgRXF1YWx-QX57XG4gICAgQ2h1bmtbQTogRXF1YWxdXG4gICAgRWl0aGVyW0E6IEVxdWFsLCBCOiBFcXVhbF1cbiAgICBFeGl0W0U6IEVxdWFsLCBBOiBFcXVhbF1cbiAgICBGW0E6IEVxdWFsXTogRGVyaXZlRXF1YWxbX11cbiAgICBMaXN0W0E6IEVxdWFsXVxuICAgIE1hcFtBLCBCOiBFcXVhbF1cbiAgICBOb25FbXB0eUNodW5rW0E6IEVxdWFsXVxuICAgIE5vbkVtcHR5TGlzdFtBOiBFcXVhbF1cbiAgICBPcHRpb25bQTogRXF1YWxdXG4gICAgVGhlc2VbQTogRXF1YWwsIEI6IEVxdWFsXVxuICAgIFRyeVtBOiBFcXVhbF1cbiAgICDinahUMTogRXF1YWwsIC4uLiwgVDIyOiBFcXVhbOKdqVxuICAgIFZhbGlkYXRpb25bRSwgQTogRXF1YWxdXG4gICAgVmVjdG9yW0E6IEVxdWFsXVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBFcXVhbF1cbiAgICBaU2V0W0EsIEI6IEhhc2hdXG5cbiAgICAoKSBib3RoW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbKEEsIEIpXVxuICAgICgpIGJvdGhXaXRoW0IsIENdKD0-IEVxdWFsW0JdKShDID0-IChBLCBCKSk6IEVxdWFsW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEVxdWFsW0JdXG4gICAgKCkgZWl0aGVyW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gRXF1YWxbQl0pKEMgPT4gRWl0aGVyW0EsIEJdKTogRXF1YWxbQ11cbiAgICAoKSBlcXVhbChBLCBBKTogQm9vbGVhblxuICAgICgpIG5vdEVxdWFsKEEsIEEpOiBCb29sZWFuXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5FcXVpdltBMV1cbiAgfVxuICBjbGFzcyBIYXNofi1BfiB7XG4gICAgQm9vbGVhblxuICAgIEJ5dGVcbiAgICBDYXVzZVtBXVxuICAgIENoYXJcbiAgICBDaHVua1tBOiBIYXNoXVxuICAgIENsYXNzW19dXG4gICAgRG91YmxlXG4gICAgRWl0aGVyW0E6IEhhc2gsIEI6IEhhc2hdXG4gICAgRltBOiBIYXNoXTogRGVyaXZlW18sIEhhc2hdXG4gICAgRmliZXIuSWRcbiAgICBGbG9hdFxuICAgIEludFxuICAgIExpc3RbQTogSGFzaF1cbiAgICBMb25nXG4gICAgTWFwW0EsIEI6IEhhc2hdXG4gICAgTm9uRW1wdHlDaHVua1tBOiBIYXNoXVxuICAgIE5vbkVtcHR5TGlzdFtBOiBIYXNoXVxuICAgIE5vbkVtcHR5U2V0W0FdXG4gICAgTm90aGluZ1xuICAgIE9wdGlvbltBOiBIYXNoXVxuICAgIE9yZGVyaW5nXG4gICAgU2V0W0FdXG4gICAgU2hvcnRcbiAgICBTdHJpbmdcbiAgICBUaGVzZVtBOiBIYXNoLCBCOiBIYXNoXVxuICAgIFRocm93YWJsZVxuICAgIOKdqFQxOiBIYXNoLCAuLi4sIFQyMjogSGFzaOKdqVxuICAgIFVuaXRcbiAgICBWYWxpZGF0aW9uW0U6IEhhc2gsIEE6IEhhc2hdXG4gICAgVmVjdG9yW0E6IEhhc2hdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEhhc2hdXG4gICAgWlNldFtBLCBCOiBIYXNoXVxuICAgIFpUcmFjZVxuXG4gICAgKCkgYm90aFtCXShIYXNoW0JdKTogSGFzaFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiAoQSwgQikpOiBIYXNoW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEhhc2hbQl1cbiAgICAoKSBlaXRoZXJbQl0oSGFzaFtCXSk6IEhhc2hbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oSGFzaFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBIYXNoW0NdXG4gICAgKCkgaGFzaChBKTogSW50XG4gIH1cbiAgY2xhc3MgT3Jkfi1BfiB7XG4gICAgQm9vbGVhblxuICAgIEJ5dGVcbiAgICBDaGFyXG4gICAgQ2h1bmtbQTogT3JkXVxuICAgIERvdWJsZVxuICAgIEVpdGhlcltBOiBPcmQsIEI6IE9yZF1cbiAgICBGW0E6IE9yZF06IERlcml2ZVtfLCBPcmRdXG4gICAgRmliZXIuSWRcbiAgICBGbG9hdFxuICAgIEludFxuICAgIExpc3RbQTogT3JkXVxuICAgIExvbmdcbiAgICBOb25FbXB0eUNodW5rW0E6IE9yZF1cbiAgICBOb25FbXB0eUxpc3RbQTogT3JkXVxuICAgIE5vdGhpbmdcbiAgICBPcHRpb25bQTogT3JkXVxuICAgIE9yZGVyaW5nXG4gICAgU2hvcnRcbiAgICBTdHJpbmdcbiAgICDinahUMTogT3JkLCAuLi4sIFQyMjogT3Jk4p2pXG4gICAgVW5pdFxuICAgIFZlY3RvcltBOiBPcmRdXG5cbiAgICAoKSBib3RoW0JdKD0-IE9yZFtCXSk6IE9yZFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IChBLCBCKSk6IE9yZFtDXVxuICAgICgpIGNvbXBhcmUoQSwgQSk6IE9yZGVyaW5nXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IE9yZFtCXVxuICAgICgpIGVpdGhlcltCXSg9PiBPcmRbQl0pOiBPcmRbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IEVpdGhlcltBLCBCXSk6IE9yZFtDXVxuICAgICgpIG1hcE9yZGVyaW5nKE9yZGVyaW5nID0-IE9yZGVyaW5nKTogT3JkW0FdXG4gICAgKCkgcmV2ZXJzZTogT3JkW0FdXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5PcmRlcmluZ1tBMV1cbiAgfSIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19fQ


# Associative

[![Associative hierarchy][Associative-image]][Associative-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  Associative~A~ <|-- Commutative~A~
  Associative~A~ <|-- Idempotent~A~
  Associative~A~ <|-- Identity~A~
  Identity~A~ <|-- InverseNonZero~A~
  InverseNonZero~A~ <|-- Inverse~A~
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
  class InverseNonZero~A~{
    F[A: InverseNonZero]: Derive[_, InverseNonZero]
    Prod[Double/Float]
    ❨T1: InverseNonZero, ..., T22: InverseNonZero❩

    () inverse(=> A, => A): A
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
  }
```

</details>

[Associative-image]: https://user-images.githubusercontent.com/9019485/96919210-bdba5300-14ab-11eb-9b2b-d4cb52e95789.png
[Associative-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gQ29tbXV0YXRpdmV-QX5cbiAgQXNzb2NpYXRpdmV-QX4gPHwtLSBJZGVtcG90ZW50fkF-XG4gIEFzc29jaWF0aXZlfkF-IDx8LS0gSWRlbnRpdHl-QX5cbiAgSWRlbnRpdHl-QX4gPHwtLSBJbnZlcnNlTm9uWmVyb35BflxuICBJbnZlcnNlTm9uWmVyb35BfiA8fC0tIEludmVyc2V-QX5cbiAgY2xhc3MgQXNzb2NpYXRpdmV-QX57XG4gICAgRWl0aGVyW0UsIEE6IEFzc29jaWF0aXZlXVxuICAgIEZbQTogQXNzb2NpYXRpdmVdOiBEZXJpdmVbXywgQXNzb2NpYXRpdmVdXG4gICAgRmlyc3RbQV1cbiAgICBMYXN0W0FdXG4gICAgTm9uRW1wdHlDaHVua1tBXVxuICAgIE5vbkVtcHR5TGlzdFtBXVxuICAgIFRoZXNlW0E6IEFzc29jaWF0aXZlLCBCOiBBc3NvY2lhdGl2ZV1cbiAgICDinahUMTogQXNzb2NpYXRpdmUsIC4uLiwgVDIyOiBBc3NvY2lhdGl2ZeKdqVxuICAgIFZhbGlkYXRpb25bRSwgQTogQXNzb2NpYXRpdmVdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IEFzc29jaWF0aXZlXVxuXG4gICAgKCkgY29tYmluZSg9PiBBLCA9PiBBKTogQVxuICB9XG4gIGNsYXNzIENvbW11dGF0aXZlfkF-e1xuICAgIEFuZFxuICAgIEZbQTogQ29tbXV0YXRpdmVdOiBEZXJpdmVbXywgQ29tbXV0YXRpdmVdXG4gICAgRWl0aGVyW0U6IENvbW11dGF0aXZlLCBBOiBDb21tdXRhdGl2ZV1cbiAgICBPclxuICAgIE1hcFtLLCBWOiBDb21tdXRhdGl2ZV1cbiAgICBNYXhbQTogT3JkXVxuICAgIE1heFtCb29sZWFuXVxuICAgIE1heFtCeXRlL0NoYXIvRG91YmxlL0Zsb2F0L0ludC9Mb25nL1Nob3J0XVxuICAgIE1pbltBOiBPcmRdXG4gICAgTWluW0Jvb2xlYW5dXG4gICAgTWluW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgTm9uRW1wdHlTZXRbQV1cbiAgICBPcHRpb25bQTogQ29tbXV0YXRpdmVdXG4gICAgUHJvZFtCb29sZWFuXVxuICAgIFByb2RbQnl0ZS9DaGFyL0RvdWJsZS9GbG9hdC9JbnQvTG9uZy9TaG9ydF1cbiAgICBTZXRbQV1cbiAgICBTdW1bQm9vbGVhbl1cbiAgICBTdW1bQnl0ZS9DaGFyL0RvdWJsZS9GbG9hdC9JbnQvTG9uZy9TaG9ydF1cbiAgICBUaGVzZVtBOiBDb21tdXRhdGl2ZSwgQjogQ29tbXV0YXRpdmVdXG4gICAg4p2oVDE6IENvbW11dGF0aXZlLCAuLi4sIFQyMjogQ29tbXV0YXRpdmXinalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IENvbW11dGF0aXZlXVxuICAgIFpTZXRbQSwgQjogQ29tbXV0YXRpdmVdXG4gICAgWk5vbkVtcHR5U2V0W0EsIEI6IENvbW11dGF0aXZlXVxuXG4gICAgKCkgY29tbXV0ZTogQ29tbXV0YXRpdmVbQV1cbiAgfVxuICBjbGFzcyBJZGVtcG90ZW50fkF-e1xuICAgIEFuZFxuICAgIEZbQTogSWRlbXBvdGVudF06IERlcml2ZVtfLCBJZGVtcG90ZW50XVxuICAgIE9yXG4gICAgTWFwW0ssIFY6IElkZW1wb3RlbnRdXG4gICAgTWF4W0Jvb2xlYW5dXG4gICAgTWF4W0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgTWluW0Jvb2xlYW5dXG4gICAgTWluW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgTm9uRW1wdHlTZXRbQV1cbiAgICBPcHRpb25bQTogSWRlbXBvdGVudF1cbiAgICBQcm9kW0Jvb2xlYW5dXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgVGhlc2VbQTogSWRlbXBvdGVudCwgQjogSWRlbXBvdGVudF1cbiAgICDinahUMTogSWRlbXBvdGVudCwgLi4uLCBUMjI6IElkZW1wb3RlbnTinalcbiAgICBWYWxpZGF0aW9uW0UsIEE6IElkZW1wb3RlbnRdXG4gICAgWlNldFtBLCBCOiBJZGVtcG90ZW50XVxuICAgIFpOb25FbXB0eVNldFtBLCBCOiBJZGVtcG90ZW50XVxuXG4gICAgKCkgY29tYmluZUlkZW1wb3RlbnQoPT4gQSwgPT4gQSkoRXF1YWxbQV0pOiBBXG4gICAgKCkgaWRlbXBvdGVudChFcXVhbFtBXSk6IElkZW1wb3RlbnRbQV1cbiAgfVxuICBjbGFzcyBJZGVudGl0eX5BfntcbiAgICBGW0E6IElkZW50aXR5XTogRGVyaXZlW18sIElkZW50aXR5XVxuICAgIENodW5rW0FdXG4gICAgRWl0aGVyW0UsIEE6IElkZW50aXR5XVxuICAgIExpc3RbQV1cbiAgICBNYXBbSywgVjogQXNzb2NpYXRpdmVdXG4gICAgTWF4W0Jvb2xlYW5dXG4gICAgTWF4W0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgTWluW0Jvb2xlYW5dXG4gICAgTWluW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAgT3B0aW9uW0E6IEFzc29jaWF0aXZlXVxuICAgIFByb2RbQnl0ZS9DaGFyL0RvdWJsZS9GbG9hdC9JbnQvTG9uZy9TaG9ydF1cbiAgICBTdHJpbmdcbiAgICDinahUMTogSWRlbnRpdHksIC4uLiwgVDIyOiBJZGVudGl0eeKdqVxuICAgIFZhbGlkYXRpb25bRSwgQTogSWRlbnRpdHldXG4gICAgVmVjdG9yW0FdXG4gICAgWlNldFtBLCBCOiBBc3NvY2lhdGl2ZV1cblxuICAgICgpIGlkZW50aXR5OiBBXG4gIH1cbiAgY2xhc3MgSW52ZXJzZU5vblplcm9-QX57XG4gICAgRltBOiBJbnZlcnNlTm9uWmVyb106IERlcml2ZVtfLCBJbnZlcnNlTm9uWmVyb11cbiAgICBQcm9kW0RvdWJsZS9GbG9hdF1cbiAgICDinahUMTogSW52ZXJzZU5vblplcm8sIC4uLiwgVDIyOiBJbnZlcnNlTm9uWmVyb-KdqVxuXG4gICAgKCkgaW52ZXJzZSg9PiBBLCA9PiBBKTogQVxuICB9XG4gIGNsYXNzIEludmVyc2V-QX57XG4gICAgQW5kXG4gICAgRltBOiBJbnZlcnNlXTogRGVyaXZlW18sIEludmVyc2VdXG4gICAgT3JcbiAgICBQcm9kW0Jvb2xlYW5dXG4gICAgU2V0W0FdXG4gICAgU3VtW0Jvb2xlYW5dXG4gICAgU3VtW0J5dGUvQ2hhci9Eb3VibGUvRmxvYXQvSW50L0xvbmcvU2hvcnRdXG4gICAg4p2oVDE6IEludmVyc2UsIC4uLiwgVDIyOiBJbnZlcnNl4p2pXG4gIH1cbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ


# AddMultiply

[![AddMultiply hierarchy][AddMultiply-image]][AddMultiply-link]

<details><summary>Mermaid</summary>

```mermaid
classDiagram
  AddMultiply~A, +Addition <: Associative, +Multiplication <: Associative~ <|-- AnnihilatingZero~A, +Addition <: Identity, +Multiplication <: Associative~
  AddMultiply~A~ <|-- DistributiveMultiply~A, +Addition <: Associative, +Multiplication <: Associative~
  AddMultiply~A~ <|-- Divide~A, +Addition <: Associative, +Multiplication <: InverseNonZero~
  AddMultiply~A~ <|-- Subtract~A, +Addition <: Inverse, +Multiplication <: Associative~
  class AddMultiply~A~{
    () add(=> A, => A): A
    () multiply(=> A, => A): A
    () Addition: Addition[Sum[A]]
    () Multiplication: Multiplication[Prod[A]]
  }
  class AnnihilatingZero~A~{
    Double [Addition = Commutative & Inverse, Multiplication = Commutative & InverseNonZero]
    Int [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]
  }
  class DistributiveMultiply~A~{
    Double [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]
    Int [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]
  }
  class Divide~A~{
    Double [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]

    () divide(=> A, => A): A
  }
  class Subtract~A~{
    Double [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]
    Int [Addition = Commutative & Inverse, Multiplication = Commutative & Identity]

    () subtract(=> A, => A): A
  }
```

</details>

[AddMultiply-image]: https://user-images.githubusercontent.com/9019485/96919033-792eb780-14ab-11eb-81a2-441a2408780b.png
[AddMultiply-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEFkZE11bHRpcGx5fkEsICtBZGRpdGlvbiA8OiBBc3NvY2lhdGl2ZSwgK011bHRpcGxpY2F0aW9uIDw6IEFzc29jaWF0aXZlfiA8fC0tIEFubmloaWxhdGluZ1plcm9-QSwgK0FkZGl0aW9uIDw6IElkZW50aXR5LCArTXVsdGlwbGljYXRpb24gPDogQXNzb2NpYXRpdmV-XG4gIEFkZE11bHRpcGx5fkF-IDx8LS0gRGlzdHJpYnV0aXZlTXVsdGlwbHl-QSwgK0FkZGl0aW9uIDw6IEFzc29jaWF0aXZlLCArTXVsdGlwbGljYXRpb24gPDogQXNzb2NpYXRpdmV-XG4gIEFkZE11bHRpcGx5fkF-IDx8LS0gRGl2aWRlfkEsICtBZGRpdGlvbiA8OiBBc3NvY2lhdGl2ZSwgK011bHRpcGxpY2F0aW9uIDw6IEludmVyc2VOb25aZXJvflxuICBBZGRNdWx0aXBseX5BfiA8fC0tIFN1YnRyYWN0fkEsICtBZGRpdGlvbiA8OiBJbnZlcnNlLCArTXVsdGlwbGljYXRpb24gPDogQXNzb2NpYXRpdmV-XG4gIGNsYXNzIEFkZE11bHRpcGx5fkF-e1xuICAgICgpIGFkZCg9PiBBLCA9PiBBKTogQVxuICAgICgpIG11bHRpcGx5KD0-IEEsID0-IEEpOiBBXG4gICAgKCkgQWRkaXRpb246IEFkZGl0aW9uW1N1bVtBXV1cbiAgICAoKSBNdWx0aXBsaWNhdGlvbjogTXVsdGlwbGljYXRpb25bUHJvZFtBXV1cbiAgfVxuICBjbGFzcyBBbm5paGlsYXRpbmdaZXJvfkF-e1xuICAgIERvdWJsZSBbQWRkaXRpb24gPSBDb21tdXRhdGl2ZSAmIEludmVyc2UsIE11bHRpcGxpY2F0aW9uID0gQ29tbXV0YXRpdmUgJiBJbnZlcnNlTm9uWmVyb11cbiAgICBJbnQgW0FkZGl0aW9uID0gQ29tbXV0YXRpdmUgJiBJbnZlcnNlLCBNdWx0aXBsaWNhdGlvbiA9IENvbW11dGF0aXZlICYgSWRlbnRpdHldXG4gIH1cbiAgY2xhc3MgRGlzdHJpYnV0aXZlTXVsdGlwbHl-QX57XG4gICAgRG91YmxlIFtBZGRpdGlvbiA9IENvbW11dGF0aXZlICYgSW52ZXJzZSwgTXVsdGlwbGljYXRpb24gPSBDb21tdXRhdGl2ZSAmIElkZW50aXR5XVxuICAgIEludCBbQWRkaXRpb24gPSBDb21tdXRhdGl2ZSAmIEludmVyc2UsIE11bHRpcGxpY2F0aW9uID0gQ29tbXV0YXRpdmUgJiBJZGVudGl0eV1cbiAgfVxuICBjbGFzcyBEaXZpZGV-QX57XG4gICAgRG91YmxlIFtBZGRpdGlvbiA9IENvbW11dGF0aXZlICYgSW52ZXJzZSwgTXVsdGlwbGljYXRpb24gPSBDb21tdXRhdGl2ZSAmIElkZW50aXR5XVxuXG4gICAgKCkgZGl2aWRlKD0-IEEsID0-IEEpOiBBXG4gIH1cbiAgY2xhc3MgU3VidHJhY3R-QX57XG4gICAgRG91YmxlIFtBZGRpdGlvbiA9IENvbW11dGF0aXZlICYgSW52ZXJzZSwgTXVsdGlwbGljYXRpb24gPSBDb21tdXRhdGl2ZSAmIElkZW50aXR5XVxuICAgIEludCBbQWRkaXRpb24gPSBDb21tdXRhdGl2ZSAmIEludmVyc2UsIE11bHRpcGxpY2F0aW9uID0gQ29tbXV0YXRpdmUgJiBJZGVudGl0eV1cblxuICAgICgpIHN1YnRyYWN0KD0-IEEsID0-IEEpOiBBXG4gIH1cbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ
