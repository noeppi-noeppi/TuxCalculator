# TuxCalculator

TuxCalculator is a calculator using GTK. It is written in Scala. It has a german Help page that explains almost everything. Not only can it calculate stuff, it also can give you the german name of almost any number.

To run it you need to have `java-gnome` installed. Sadly it is only available on linux. To install it on ubuntu:

```
sudo apt install libjava-gnome-java libjava-gnome-jni
```

The program will automatically find the library at `/usr/share/java/gtk.jar` (its default installation path). If you're not on linux, you can still use the text based mode. To disable the GUI call the calculater with the `nogui` Argument (without a preceding hyphen)

Did you know that a one with 12345678 zeros is called `billiseptenquinquagintallitredeziseszentillion` in german?