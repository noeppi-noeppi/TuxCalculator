# TuxCalculator

*TuxCalculator* is a powerful text-based calculator.
That means, expressions are typed in as text and then evaluated to give a result.

*TuxCalculator* is written in Scala and runs on Desktop as well as Android.
It can use either GTK (through [java-gnome](https://java-gnome.sourceforge.net/)), JavaFX or Swing or run entirely text based.

To get started, head over to [Releases](https://github.com/noeppi-noeppi/TuxCalculator/releases/latest).
There you can download the *fatjar* for desktop or the *apk* file to install on an Android phone.
There is also a manual that explains TuxCalculator in detail.

### Features

Briefly lists some of the features.
You should read the [manual](https://github.com/noeppi-noeppi/TuxCalculator/releases/latest) for full documentation.

  * Supports complex numbers: `(2 + 3*i) * sqrt(-7)`
  * Change the decimal precision used to *compute* results to any number: `set precision = 36`
  * Change the decimal precision used to *output* results to any number: `set output = 36`
  * Definition of custom functions: `def f(x) = (x-1)^2`
  * Lists and argument splatting: `def maxOfList(l) = max(l...)`
  * Vectors and matrices: `#[1,3;2,5] * #[4, -1]`
  * Lambdas: `(\x -> 2 * x)(8) = 16`
  * Unicode: `[1,2,3] ∪ [2,3,4]`, `(false ⇒ true) = true`

For more examples, take a look at the [plain.tuxc](./TuxCore/src/dev/resources/tuxcalculator/plain.tuxc) file, which defines all the builtin functions and operators.

![](https://user-images.githubusercontent.com/63002502/233791457-543e8c06-f2d1-4b71-ae08-b0a8a9ad5f89.png)
