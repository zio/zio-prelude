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
    F[A: Equal]: DeriveEqual
    Either[A: Equal, B: Equal]
    Exit[E: Equal, A: Equal]
    List[A: Equal]
    Map[A, B: Equal]
    NonEmptyChunk[A: Equal]
    Option[A: Equal]
    Try[A: Equal]
    ⟮A: Equal, ..., Z: Equal⟯
    Vector[A: Equal]

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
    Class[_]
    Double
    Fiber.Id
    Float
    Int
    Long
    Nothing
    Set[A]
    Short
    String
    Throwable
    Unit
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
    Double
    Fiber.Id
    Float
    Int
    Long
    Nothing
    Short
    String
    Unit

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

[Equal-image]: https://user-images.githubusercontent.com/9019485/94750410-44d14b00-0386-11eb-91a3-001b03ca98ff.png
[Equal-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gICAgRXF1YWx-LUF-IDx8LS0gSGFzaH4tQX5cbiAgICBFcXVhbH4tQX4gPHwtLSBPcmR-LUF-XG4gICAgY2xhc3MgRXF1YWx-QX57XG4gICAgICBDaHVua1tBOiBFcXVhbF1cbiAgICAgIEZbQTogRXF1YWxdOiBEZXJpdmVFcXVhbFxuICAgICAgRWl0aGVyW0E6IEVxdWFsLCBCOiBFcXVhbF1cbiAgICAgIEV4aXRbRTogRXF1YWwsIEE6IEVxdWFsXVxuICAgICAgTGlzdFtBOiBFcXVhbF1cbiAgICAgIE1hcFtBLCBCOiBFcXVhbF1cbiAgICAgIE5vbkVtcHR5Q2h1bmtbQTogRXF1YWxdXG4gICAgICBPcHRpb25bQTogRXF1YWxdXG4gICAgICBUcnlbQTogRXF1YWxdXG4gICAgICDin65BOiBFcXVhbCwgLi4uLCBaOiBFcXVhbOKfr1xuICAgICAgVmVjdG9yW0E6IEVxdWFsXVxuXG4gICAgICAoKSBib3RoW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbKEEsIEIpXVxuICAgICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gRXF1YWxbQl0pKEMgPT4gKEEsIEIpKTogRXF1YWxbQ11cbiAgICAgICgpIGNvbnRyYW1hcFtCXShCID0-IEEpOiBFcXVhbFtCXVxuICAgICAgKCkgZWl0aGVyW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbRWl0aGVyW0EsIEJdXVxuICAgICAgKCkgZWl0aGVyV2l0aFtCLCBDXSg9PiBFcXVhbFtCXSkoQyA9PiBFaXRoZXJbQSwgQl0pOiBFcXVhbFtDXVxuICAgICAgKCkgZXF1YWwoQSwgQSk6IEJvb2xlYW5cbiAgICAgICgpIG5vdEVxdWFsKEEsIEEpOiBCb29sZWFuXG4gICAgICAoKSB0b1NjYWxhW0ExIDw6IEFdOiBzY2FsYS5tYXRoLkVxdWl2W0ExXVxuICAgIH1cbiAgICBjbGFzcyBIYXNofi1BfiB7XG4gICAgICBCb29sZWFuXG4gICAgICBCeXRlXG4gICAgICBDYXVzZVtBXVxuICAgICAgQ2hhclxuICAgICAgQ2xhc3NbX11cbiAgICAgIERvdWJsZVxuICAgICAgRmliZXIuSWRcbiAgICAgIEZsb2F0XG4gICAgICBJbnRcbiAgICAgIExvbmdcbiAgICAgIE5vdGhpbmdcbiAgICAgIFNldFtBXVxuICAgICAgU2hvcnRcbiAgICAgIFN0cmluZ1xuICAgICAgVGhyb3dhYmxlXG4gICAgICBVbml0XG4gICAgICBaVHJhY2VcblxuICAgICAgKCkgYm90aFtCXShIYXNoW0JdKTogSGFzaFsoQSwgQildXG4gICAgICAoKSBib3RoV2l0aFtCLCBDXShIYXNoW0JdKShDID0-IChBLCBCKSk6IEhhc2hbQ11cbiAgICAgICgpIGNvbnRyYW1hcFtCXShCID0-IEEpOiBIYXNoW0JdXG4gICAgICAoKSBlaXRoZXJbQl0oSGFzaFtCXSk6IEhhc2hbRWl0aGVyW0EsIEJdXVxuICAgICAgKCkgZWl0aGVyV2l0aFtCLCBDXShIYXNoW0JdKShDID0-IEVpdGhlcltBLCBCXSk6IEhhc2hbQ11cbiAgICAgICgpIGhhc2goQSk6IEludFxuICAgIH1cbiAgICBjbGFzcyBPcmR-LUF-IHtcbiAgICAgIEJvb2xlYW5cbiAgICAgIEJ5dGVcbiAgICAgIENoYXJcbiAgICAgIERvdWJsZVxuICAgICAgRmliZXIuSWRcbiAgICAgIEZsb2F0XG4gICAgICBJbnRcbiAgICAgIExvbmdcbiAgICAgIE5vdGhpbmdcbiAgICAgIFNob3J0XG4gICAgICBTdHJpbmdcbiAgICAgIFVuaXRcblxuICAgICAgKCkgYm90aFtCXSg9PiBPcmRbQl0pOiBPcmRbKEEsIEIpXVxuICAgICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IChBLCBCKSk6IE9yZFtDXVxuICAgICAgKCkgY29tcGFyZShBLCBBKTogT3JkZXJpbmdcbiAgICAgICgpIGNvbnRyYW1hcFtCXShCID0-IEEpOiBPcmRbQl1cbiAgICAgICgpIGVpdGhlcltCXSg9PiBPcmRbQl0pOiBPcmRbRWl0aGVyW0EsIEJdXVxuICAgICAgKCkgZWl0aGVyV2l0aFtCLCBDXSg9PiBPcmRbQl0pKEMgPT4gRWl0aGVyW0EsIEJdKTogT3JkW0NdXG4gICAgICAoKSBtYXBPcmRlcmluZyhPcmRlcmluZyA9PiBPcmRlcmluZyk6IE9yZFtBXVxuICAgICAgKCkgcmV2ZXJzZTogT3JkW0FdXG4gICAgICAoKSB0b1NjYWxhW0ExIDw6IEFdOiBzY2FsYS5tYXRoLk9yZGVyaW5nW0ExXVxuICAgIH1cbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19fQ

</details>
