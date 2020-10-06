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
    Nothing
    Option[A: Hash]
    Set[A]
    Short
    String
    Throwable
    ⟮A: Hash, ..., Z: Hash⟯
    Unit
    Vector[A: Hash]
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
    Nothing
    Option[A: Ord]
    Short
    String
    ⟮A: Ord, ..., Z: Ord⟯
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

[Equal-image]: https://user-images.githubusercontent.com/9019485/95245237-23071680-0813-11eb-8e60-ee7ef43ece7f.png
[Equal-link]: https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiY2xhc3NEaWFncmFtXG4gIEVxdWFsfi1BfiA8fC0tIEhhc2h-LUF-XG4gIEVxdWFsfi1BfiA8fC0tIE9yZH4tQX5cbiAgY2xhc3MgRXF1YWx-QX57XG4gICAgQ2h1bmtbQTogRXF1YWxdXG4gICAgRWl0aGVyW0E6IEVxdWFsLCBCOiBFcXVhbF1cbiAgICBFeGl0W0U6IEVxdWFsLCBBOiBFcXVhbF1cbiAgICBGW0E6IEVxdWFsXTogRGVyaXZlRXF1YWxbX11cbiAgICBMaXN0W0E6IEVxdWFsXVxuICAgIE1hcFtBLCBCOiBFcXVhbF1cbiAgICBOb25FbXB0eUNodW5rW0E6IEVxdWFsXVxuICAgIE9wdGlvbltBOiBFcXVhbF1cbiAgICBUcnlbQTogRXF1YWxdXG4gICAg4p-uQTogRXF1YWwsIC4uLiwgWjogRXF1YWzin69cbiAgICBWZWN0b3JbQTogRXF1YWxdXG5cbiAgICAoKSBib3RoW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbKEEsIEIpXVxuICAgICgpIGJvdGhXaXRoW0IsIENdKD0-IEVxdWFsW0JdKShDID0-IChBLCBCKSk6IEVxdWFsW0NdXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IEVxdWFsW0JdXG4gICAgKCkgZWl0aGVyW0JdKD0-IEVxdWFsW0JdKTogRXF1YWxbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gRXF1YWxbQl0pKEMgPT4gRWl0aGVyW0EsIEJdKTogRXF1YWxbQ11cbiAgICAoKSBlcXVhbChBLCBBKTogQm9vbGVhblxuICAgICgpIG5vdEVxdWFsKEEsIEEpOiBCb29sZWFuXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5FcXVpdltBMV1cbiAgfVxuICBjbGFzcyBIYXNofi1BfiB7XG4gICAgQm9vbGVhblxuICAgIEJ5dGVcbiAgICBDYXVzZVtBXVxuICAgIENoYXJcbiAgICBDaHVua1tBOiBIYXNoXVxuICAgIENsYXNzW19dXG4gICAgRG91YmxlXG4gICAgRWl0aGVyW0E6IEhhc2gsIEI6IEhhc2hdXG4gICAgRltBOiBIYXNoXTogRGVyaXZlW18sIEhhc2hdXG4gICAgRmliZXIuSWRcbiAgICBGbG9hdFxuICAgIEludFxuICAgIExpc3RbQTogSGFzaF1cbiAgICBMb25nXG4gICAgTWFwW0EsIEI6IEhhc2hdXG4gICAgTm9uRW1wdHlDaHVua1tBOiBIYXNoXVxuICAgIE5vdGhpbmdcbiAgICBPcHRpb25bQTogSGFzaF1cbiAgICBTZXRbQV1cbiAgICBTaG9ydFxuICAgIFN0cmluZ1xuICAgIFRocm93YWJsZVxuICAgIOKfrkE6IEhhc2gsIC4uLiwgWjogSGFzaOKfr1xuICAgIFVuaXRcbiAgICBWZWN0b3JbQTogSGFzaF1cbiAgICBaVHJhY2VcblxuICAgICgpIGJvdGhbQl0oSGFzaFtCXSk6IEhhc2hbKEEsIEIpXVxuICAgICgpIGJvdGhXaXRoW0IsIENdKEhhc2hbQl0pKEMgPT4gKEEsIEIpKTogSGFzaFtDXVxuICAgICgpIGNvbnRyYW1hcFtCXShCID0-IEEpOiBIYXNoW0JdXG4gICAgKCkgZWl0aGVyW0JdKEhhc2hbQl0pOiBIYXNoW0VpdGhlcltBLCBCXV1cbiAgICAoKSBlaXRoZXJXaXRoW0IsIENdKEhhc2hbQl0pKEMgPT4gRWl0aGVyW0EsIEJdKTogSGFzaFtDXVxuICAgICgpIGhhc2goQSk6IEludFxuICB9XG4gIGNsYXNzIE9yZH4tQX4ge1xuICAgIEJvb2xlYW5cbiAgICBCeXRlXG4gICAgQ2hhclxuICAgIENodW5rW0E6IE9yZF1cbiAgICBEb3VibGVcbiAgICBFaXRoZXJbQTogT3JkLCBCOiBPcmRdXG4gICAgRltBOiBPcmRdOiBEZXJpdmVbXywgT3JkXVxuICAgIEZpYmVyLklkXG4gICAgRmxvYXRcbiAgICBJbnRcbiAgICBMaXN0W0E6IE9yZF1cbiAgICBMb25nXG4gICAgTm9uRW1wdHlDaHVua1tBOiBPcmRdXG4gICAgTm90aGluZ1xuICAgIE9wdGlvbltBOiBPcmRdXG4gICAgU2hvcnRcbiAgICBTdHJpbmdcbiAgICDin65BOiBPcmQsIC4uLiwgWjogT3Jk4p-vXG4gICAgVW5pdFxuICAgIFZlY3RvcltBOiBPcmRdXG5cbiAgICAoKSBib3RoW0JdKD0-IE9yZFtCXSk6IE9yZFsoQSwgQildXG4gICAgKCkgYm90aFdpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IChBLCBCKSk6IE9yZFtDXVxuICAgICgpIGNvbXBhcmUoQSwgQSk6IE9yZGVyaW5nXG4gICAgKCkgY29udHJhbWFwW0JdKEIgPT4gQSk6IE9yZFtCXVxuICAgICgpIGVpdGhlcltCXSg9PiBPcmRbQl0pOiBPcmRbRWl0aGVyW0EsIEJdXVxuICAgICgpIGVpdGhlcldpdGhbQiwgQ10oPT4gT3JkW0JdKShDID0-IEVpdGhlcltBLCBCXSk6IE9yZFtDXVxuICAgICgpIG1hcE9yZGVyaW5nKE9yZGVyaW5nID0-IE9yZGVyaW5nKTogT3JkW0FdXG4gICAgKCkgcmV2ZXJzZTogT3JkW0FdXG4gICAgKCkgdG9TY2FsYVtBMSA8OiBBXTogc2NhbGEubWF0aC5PcmRlcmluZ1tBMV1cbiAgfVxuIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQiLCJ0aGVtZVZhcmlhYmxlcyI6eyJiYWNrZ3JvdW5kIjoid2hpdGUiLCJwcmltYXJ5Q29sb3IiOiIjRUNFQ0ZGIiwic2Vjb25kYXJ5Q29sb3IiOiIjZmZmZmRlIiwidGVydGlhcnlDb2xvciI6ImhzbCg4MCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwicHJpbWFyeUJvcmRlckNvbG9yIjoiaHNsKDI0MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJzZWNvbmRhcnlCb3JkZXJDb2xvciI6ImhzbCg2MCwgNjAlLCA4My41Mjk0MTE3NjQ3JSkiLCJ0ZXJ0aWFyeUJvcmRlckNvbG9yIjoiaHNsKDgwLCA2MCUsIDg2LjI3NDUwOTgwMzklKSIsInByaW1hcnlUZXh0Q29sb3IiOiIjMTMxMzAwIiwic2Vjb25kYXJ5VGV4dENvbG9yIjoiIzAwMDAyMSIsInRlcnRpYXJ5VGV4dENvbG9yIjoicmdiKDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxLCA5LjUwMDAwMDAwMDEpIiwibGluZUNvbG9yIjoiIzMzMzMzMyIsInRleHRDb2xvciI6IiMzMzMiLCJtYWluQmtnIjoiI0VDRUNGRiIsInNlY29uZEJrZyI6IiNmZmZmZGUiLCJib3JkZXIxIjoiIzkzNzBEQiIsImJvcmRlcjIiOiIjYWFhYTMzIiwiYXJyb3doZWFkQ29sb3IiOiIjMzMzMzMzIiwiZm9udEZhbWlseSI6IlwidHJlYnVjaGV0IG1zXCIsIHZlcmRhbmEsIGFyaWFsIiwiZm9udFNpemUiOiIxNnB4IiwibGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsIm5vZGVCa2ciOiIjRUNFQ0ZGIiwibm9kZUJvcmRlciI6IiM5MzcwREIiLCJjbHVzdGVyQmtnIjoiI2ZmZmZkZSIsImNsdXN0ZXJCb3JkZXIiOiIjYWFhYTMzIiwiZGVmYXVsdExpbmtDb2xvciI6IiMzMzMzMzMiLCJ0aXRsZUNvbG9yIjoiIzMzMyIsImVkZ2VMYWJlbEJhY2tncm91bmQiOiIjZThlOGU4IiwiYWN0b3JCb3JkZXIiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJhY3RvckJrZyI6IiNFQ0VDRkYiLCJhY3RvclRleHRDb2xvciI6ImJsYWNrIiwiYWN0b3JMaW5lQ29sb3IiOiJncmV5Iiwic2lnbmFsQ29sb3IiOiIjMzMzIiwic2lnbmFsVGV4dENvbG9yIjoiIzMzMyIsImxhYmVsQm94QmtnQ29sb3IiOiIjRUNFQ0ZGIiwibGFiZWxCb3hCb3JkZXJDb2xvciI6ImhzbCgyNTkuNjI2MTY4MjI0MywgNTkuNzc2NTM2MzEyOCUsIDg3LjkwMTk2MDc4NDMlKSIsImxhYmVsVGV4dENvbG9yIjoiYmxhY2siLCJsb29wVGV4dENvbG9yIjoiYmxhY2siLCJub3RlQm9yZGVyQ29sb3IiOiIjYWFhYTMzIiwibm90ZUJrZ0NvbG9yIjoiI2ZmZjVhZCIsIm5vdGVUZXh0Q29sb3IiOiJibGFjayIsImFjdGl2YXRpb25Cb3JkZXJDb2xvciI6IiM2NjYiLCJhY3RpdmF0aW9uQmtnQ29sb3IiOiIjZjRmNGY0Iiwic2VxdWVuY2VOdW1iZXJDb2xvciI6IndoaXRlIiwic2VjdGlvbkJrZ0NvbG9yIjoicmdiYSgxMDIsIDEwMiwgMjU1LCAwLjQ5KSIsImFsdFNlY3Rpb25Ca2dDb2xvciI6IndoaXRlIiwic2VjdGlvbkJrZ0NvbG9yMiI6IiNmZmY0MDAiLCJ0YXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwidGFza0JrZ0NvbG9yIjoiIzhhOTBkZCIsInRhc2tUZXh0TGlnaHRDb2xvciI6IndoaXRlIiwidGFza1RleHRDb2xvciI6IndoaXRlIiwidGFza1RleHREYXJrQ29sb3IiOiJibGFjayIsInRhc2tUZXh0T3V0c2lkZUNvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dENsaWNrYWJsZUNvbG9yIjoiIzAwMzE2MyIsImFjdGl2ZVRhc2tCb3JkZXJDb2xvciI6IiM1MzRmYmMiLCJhY3RpdmVUYXNrQmtnQ29sb3IiOiIjYmZjN2ZmIiwiZ3JpZENvbG9yIjoibGlnaHRncmV5IiwiZG9uZVRhc2tCa2dDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQm9yZGVyQ29sb3IiOiJncmV5IiwiY3JpdEJvcmRlckNvbG9yIjoiI2ZmODg4OCIsImNyaXRCa2dDb2xvciI6InJlZCIsInRvZGF5TGluZUNvbG9yIjoicmVkIiwibGFiZWxDb2xvciI6ImJsYWNrIiwiZXJyb3JCa2dDb2xvciI6IiM1NTIyMjIiLCJlcnJvclRleHRDb2xvciI6IiM1NTIyMjIiLCJjbGFzc1RleHQiOiIjMTMxMzAwIiwiZmlsbFR5cGUwIjoiI0VDRUNGRiIsImZpbGxUeXBlMSI6IiNmZmZmZGUiLCJmaWxsVHlwZTIiOiJoc2woMzA0LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTMiOiJoc2woMTI0LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTQiOiJoc2woMTc2LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTUiOiJoc2woLTQsIDEwMCUsIDkzLjUyOTQxMTc2NDclKSIsImZpbGxUeXBlNiI6ImhzbCg4LCAxMDAlLCA5Ni4yNzQ1MDk4MDM5JSkiLCJmaWxsVHlwZTciOiJoc2woMTg4LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkifX0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9

</details>
