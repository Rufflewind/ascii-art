ASCII art generator
===================

This is a small [Haskell][1] script that converts image files into ASCII art
suitable for output in terminals that support 256-color escape codes.  The
script can be modified to use only the standard 16 colors as well.

Other than a working Haskell package (obviously), it has only one (very
important) dependency: the [`JuicyPixels` library][2], used to decode the
images.  Note that it's probably OK to use a slightly newer or older version
of this library (as well as the Haskell `base` library).  To install
JuicyPixels, run:

    cabal install JuicyPixels

Usage
-----

You can run it either directly as a script:

    runhaskell Main.hs FILE...

or as a compiled executable:

    # with Cabal
    cabal build && dist/build/ascii-art/ascii-art FILE...

    # without Cabal
    ghc -O2 -Wall Main.hs && ./Main FILE...

Here, `FILE` is an argument that specifies the path to the image file.

Input
-----

Thanks to JuicyPixels, several image formats are supported, including PNG and
JPEG.  However, only certain color formats are supported (including
8-bit/channel RGB or 8-bit grayscale, with or without alpha).

Since characters are usually longer vertically, it's probably a good idea to
resize your images to an approximate ratio of 2:1 before converting into text.

Customization
-------------

There are many ways to customize / extend this.  I'd be happy to hear what you
can come up with!  ^^

- To restrict the colors to the more portable 16-color palette, you can change
  the `convert` variable in `pixelToChar` to `approxS16`.

- Unicode would probably work fine with a bit of tweaking to the alpha
  (transparency) to character converter: `alphaToChar`.

- To change the maximum width used to center the image, you can alter the
  `maxWith` variable in `main`.  Or, set it to zero to disable centering
  altogether.

[1]: http://haskell.org
[2]: http://hackage.haskell.org/package/JuicyPixels
