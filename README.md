# Imperative Programming — Practical 1

This repository includes code that I wrote for the first practical of Imperative Programming Parts 1 and 2. It includes a method for Vigenère cipher encryption, as well as methods for decryption. One decryption method requires a crib, while the other is a statistically-based method for use when a crib is not known.

## Usage

From the command line:
* Encrypt with key PINGPONG: `scala Cipher -encrypt PINGPONG santa`
* Decrypt text file msg using crib "Dear Santa": `scala Cipher -crib "Dear Santa" msg`
* Crack key length (output number of matches for various key lengths): `scala Cipher -crackKeyLen <encrypted file>`
*NOTE: this method requires user analysis to determine which key length is the most likely*
* Crack message given a key length: `scala Cipher -crackKey <key length> <encrypted file> | sort -n | uniq -c | awk '$1 > 6'`

Encrypted files private1 and private2, as well as plaintext file santa, have been provided for testing purposes.
