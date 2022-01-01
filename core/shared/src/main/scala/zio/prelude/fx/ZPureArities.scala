/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude.fx

import zio.prelude._

trait ZPureArities {

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2]
  )(
    f: (A0, A1, A2) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17 <*> zPure18)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17 <*> zPure18 <*> zPure19)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17 <*> zPure18 <*> zPure19 <*> zPure20)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the first error if any fail.
   */
  def mapN[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    B
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17 <*> zPure18 <*> zPure19 <*> zPure20 <*> zPure21)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapN[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    B
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20],
    zPure22: ZPure[W, S, S, R, E, A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <*> zPure2 <*> zPure3 <*> zPure4 <*> zPure5 <*> zPure6 <*> zPure7 <*> zPure8 <*> zPure9 <*> zPure10 <*> zPure11 <*> zPure12 <*> zPure13 <*> zPure14 <*> zPure15 <*> zPure16 <*> zPure17 <*> zPure18 <*> zPure19 <*> zPure20 <*> zPure21 <*> zPure22)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2]
  )(
    f: (A0, A1, A2) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9).map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17 <&> zPure18)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17 <&> zPure18 <&> zPure19)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    B
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17 <&> zPure18 <&> zPure19 <&> zPure20)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    B
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17 <&> zPure18 <&> zPure19 <&> zPure20 <&> zPure21)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values using the function
   * `f`, failing with the accumulation of all errors if any fail.
   */
  def mapParN[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    B
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20],
    zPure22: ZPure[W, S, S, R, E, A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): ZPure[W, S, S, R, E, B] =
    (zPure1 <&> zPure2 <&> zPure3 <&> zPure4 <&> zPure5 <&> zPure6 <&> zPure7 <&> zPure8 <&> zPure9 <&> zPure10 <&> zPure11 <&> zPure12 <&> zPure13 <&> zPure14 <&> zPure15 <&> zPure16 <&> zPure17 <&> zPure18 <&> zPure19 <&> zPure20 <&> zPure21 <&> zPure22)
      .map(reassociate(f))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2]
  ): ZPure[W, S, S, R, E, (A0, A1, A2)] =
    mapN(zPure1, zPure2, zPure3)((_, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3)] =
    mapN(zPure1, zPure2, zPure3, zPure4)((_, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5)((_, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6)((_, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7)((_, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8)((_, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10)(
      (_, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11)(
      (_, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11, zPure12)(
      (_, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    mapN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11, zPure12, zPure13)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20]
  ): ZPure[
    W,
    S,
    S,
    R,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
  ] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20,
      zPure21
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the first error if any fail.
   */
  def tupled[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20],
    zPure22: ZPure[W, S, S, R, E, A21]
  ): ZPure[
    W,
    S,
    S,
    R,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  ] =
    mapN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20,
      zPure21,
      zPure22
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2]
  ): ZPure[W, S, S, R, E, (A0, A1, A2)] =
    mapParN(zPure1, zPure2, zPure3)((_, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3)] =
    mapParN(zPure1, zPure2, zPure3, zPure4)((_, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5)((_, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6)((_, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7)((_, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8)((_, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10)(
      (_, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11)(
      (_, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11, zPure12)(
      (_, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    mapParN(zPure1, zPure2, zPure3, zPure4, zPure5, zPure6, zPure7, zPure8, zPure9, zPure10, zPure11, zPure12, zPure13)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[W, S, R, E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19]
  ): ZPure[W, S, S, R, E, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20]
  ): ZPure[
    W,
    S,
    S,
    R,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
  ] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20,
      zPure21
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines the results of the specified `ZPure` values into a tuple, failing
   * with the accumulation of all errors if any fail.
   */
  def tupledPar[
    W,
    S,
    R,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21
  ](
    zPure1: ZPure[W, S, S, R, E, A0],
    zPure2: ZPure[W, S, S, R, E, A1],
    zPure3: ZPure[W, S, S, R, E, A2],
    zPure4: ZPure[W, S, S, R, E, A3],
    zPure5: ZPure[W, S, S, R, E, A4],
    zPure6: ZPure[W, S, S, R, E, A5],
    zPure7: ZPure[W, S, S, R, E, A6],
    zPure8: ZPure[W, S, S, R, E, A7],
    zPure9: ZPure[W, S, S, R, E, A8],
    zPure10: ZPure[W, S, S, R, E, A9],
    zPure11: ZPure[W, S, S, R, E, A10],
    zPure12: ZPure[W, S, S, R, E, A11],
    zPure13: ZPure[W, S, S, R, E, A12],
    zPure14: ZPure[W, S, S, R, E, A13],
    zPure15: ZPure[W, S, S, R, E, A14],
    zPure16: ZPure[W, S, S, R, E, A15],
    zPure17: ZPure[W, S, S, R, E, A16],
    zPure18: ZPure[W, S, S, R, E, A17],
    zPure19: ZPure[W, S, S, R, E, A18],
    zPure20: ZPure[W, S, S, R, E, A19],
    zPure21: ZPure[W, S, S, R, E, A20],
    zPure22: ZPure[W, S, S, R, E, A21]
  ): ZPure[
    W,
    S,
    S,
    R,
    E,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  ] =
    mapParN(
      zPure1,
      zPure2,
      zPure3,
      zPure4,
      zPure5,
      zPure6,
      zPure7,
      zPure8,
      zPure9,
      zPure10,
      zPure11,
      zPure12,
      zPure13,
      zPure14,
      zPure15,
      zPure16,
      zPure17,
      zPure18,
      zPure19,
      zPure20,
      zPure21,
      zPure22
    )((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  private def reassociate[A, B, C, D](function: (A, B, C) => D): (((A, B), C)) => D = { case ((a, b), c) =>
    function(a, b, c)
  }

  private def reassociate[A, B, C, D, E](function: (A, B, C, D) => E): ((((A, B), C), D)) => E = {
    case (((a, b), c), d) =>
      function(a, b, c, d)
  }

  private def reassociate[A, B, C, D, E, F](function: (A, B, C, D, E) => F): (((((A, B), C), D), E)) => F = {
    case ((((a, b), c), d), e) => function(a, b, c, d, e)
  }

  private def reassociate[A, B, C, D, E, F, G](function: (A, B, C, D, E, F) => G): ((((((A, B), C), D), E), F)) => G = {
    case (((((a, b), c), d), e), f) => function(a, b, c, d, e, f)
  }

  private def reassociate[A, B, C, D, E, F, G, H](
    function: (A, B, C, D, E, F, G) => H
  ): (((((((A, B), C), D), E), F), G)) => H = { case ((((((a, b), c), d), e), f), g) =>
    function(a, b, c, d, e, f, g)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I](
    function: (A, B, C, D, E, F, G, H) => I
  ): ((((((((A, B), C), D), E), F), G), H)) => I = { case (((((((a, b), c), d), e), f), g), h) =>
    function(a, b, c, d, e, f, g, h)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J](
    function: (A, B, C, D, E, F, G, H, I) => J
  ): (((((((((A, B), C), D), E), F), G), H), I)) => J = { case ((((((((a, b), c), d), e), f), g), h), i) =>
    function(a, b, c, d, e, f, g, h, i)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K](
    function: (A, B, C, D, E, F, G, H, I, J) => K
  ): ((((((((((A, B), C), D), E), F), G), H), I), J)) => K = { case (((((((((a, b), c), d), e), f), g), h), i), j) =>
    function(a, b, c, d, e, f, g, h, i, j)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L](
    function: (A, B, C, D, E, F, G, H, I, J, K) => L
  ): (((((((((((A, B), C), D), E), F), G), H), I), J), K)) => L = {
    case ((((((((((a, b), c), d), e), f), g), h), i), j), k) =>
      function(a, b, c, d, e, f, g, h, i, j, k)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M](
    function: (A, B, C, D, E, F, G, H, I, J, K, L) => M
  ): ((((((((((((A, B), C), D), E), F), G), H), I), J), K), L)) => M = {
    case (((((((((((a, b), c), d), e), f), g), h), i), j), k), l) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N
  ): (((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M)) => N = {
    case ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O
  ): ((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N)) => O = {
    case (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P
  ): (((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O)) => P = {
    case ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q
  ): ((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P)) => Q = {
    case (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R
  ): (((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q)) => R = {
    case ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S
  ): ((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R)) => S = {
    case (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
  ): (((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S)) => T = {
    case ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
  ): ((((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T)) => U = {
    case (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
  ): (((((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U)) => V = {
    case ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  }

  private def reassociate[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](
    function: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
  ): (
    (((((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U), V)
  ) => W = {
    case (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v) =>
      function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  }
}
