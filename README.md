# prbs

Library of pseudo-random binary sequence generators (LFSR-2 and
LFSR-4) and related functions in Common Lisp.

## Overview
 
This is a library of higher-order functions that will generate [PRBS
sequences](https://en.wikipedia.org/wiki/Pseudorandom_binary_sequence)
of degree [3 through 786, 1024, 2048, and
4096](https://pdfs.semanticscholar.org/9487/ab51297b35a1ff570db6f7223870d8da7fa9.pdf).
The closures produced by these functions will generate sequences as
bitvectors, byte arrays, lists of bit-vectors, or lists of unsigned
integers.

Other functions are included for detecting and tracking errors in PRBS
data and finding the degree of unknown PRBS data.

## Sequence Generation

The smallest PRBS sequence is PRBS-3, which is only 21 bits long.

~~~lisp
(ql:quickload :prbs)
(use-package :prbs)

(funcall (bit-gen 3) 21)
=> #*010101011111110100001

~~~

Once a generator is created, subsequent calls to it will generate more data. Sequences repeat once they reach the end.

~~~lisp
(defvar gen (num-gen 5))
=> GEN

(funcall gen 20)
=> (2 5 11 22 13 26 21 10 20 8 17 3 7 14 29 27 23 15 31 30)

(funcall gen 20)
=> (28 25 18 4 9 19 6 12 24 16 1 2 5 11 22 13 26 21 10 20)

~~~

Generators can be seeded with an integer value such as the current
time in seconds. Otherwise they are seeded with the value 2, which makes the
final value in the sequence always be 1. This is convenient for
testing purposes but it makes early values in the sequence "zero heavy".

~~~lisp
(defvar bytes (byte-gen 31))
=> BYTES

(funcall bytes 20)
=> #(0 0 0 4 0 0 0 16 0 0 0 64 0 0 1 0 0 0 4 0)

(setq bytes (byte-gen 31 :seed (get-universal-time)))
=> #<CLOSURE (LAMBDA (&OPTIONAL (PRBS::C 1)) :IN BYTE-GEN) {10030EA65B}>

(funcall bytes 20)
=> #(181 114 141 52 213 202 52 215 87 40 211 85 92 163 77 85 114 141 53 85)

~~~

## Bit Error Detection

PRBS sequences are often used to characterize the error rate in
communication links. If you generate packets of PRBS data and send
them across an unreliable link, the received packets can be analyzed
to determine the error rate of the comm link.

Assume that you have created packets of data for transmission:

~~~lisp
(let ((gen (byte-gen 33 :seed (get-universal-time))))
     ...
     (transmit (funcall gen 40))
     ...
     (transmit (funcall gen 40))
     ...

~~~     

In this case, 40-byte packets from PRBS-33 are being
transmitted. Across the link, once any packet is received it can be
used to "lock" on the PRBS sequence:

~~~lisp
(ql:quickload :prbs)
(use-package :prbs.err)

(let ((tracker (prbs-lock (receive-packet-data ...) 33)))

~~~

The prbs-lock function will use the available data to locate where it is in
the sequence and initialize a PRBS generator of its own that it uses
to predict future data. Any arriving data that does not match the
prediction is considered an error.

~~~lisp
       (funcall tracker (receive-packet-data ...))
=> total-errors total-bits
       ...
       (funcall tracker (receive-packet-data ...))
=> total-errors total-bits
       ...

~~~

See sender.ros and recv.ros in the test folder for an example that does this with UDP
datagrams.

## Sequence Detection

Someday you might encounter some data that you suspect is from a PRBS, but you are not sure which one.

~~~lisp
(ql:quickload :prbs)
(use-package '(prbs prbs.err prbs.util) :cl-user)

(prbs-detect (bytes->bits (loop repeat 75 collect (random 255))) :max 100)
=> NIL

(prbs-detect (take 600 (bit-gen 45 :seed (get-universal-time) :start 11)) :max 100)
=> (45)

~~~

As expected, a 75-byte sample of random data does not match any PRBS
sequence.  However, a 75-byte sample (600 bits) from PRBS-45 taken at
an arbitrary non-aligned bit offset is enough to uniquely identify
the sequence -- not bad considering the full PRBS-45 sequence is
almost 200 terabytes long.

## API Reference

[API Reference](http://htmlpreview.github.com/?http://github.com/jlowder/prbs/blob/master/doc/ref.html)

## License

MIT
