# notes
Goal is to sum up all version numbers in all packets in the hierarchy. I don't actually need to do anything other than figure out version numbers and how to traverse the hierarchy.

# Running
`
> racket part1.rkt <input-file>
`

# Run tests
`
> racket part1-tests.rkt
`

# TODOs
* [X] read literal outputs struct
* [I] test for embedded packets
* [ ] test for recursively embedded packets?
* [ ] test for very long literal

# Thoughts on Parsing
Know that after coming back from recursing on a sub-section we should know how far this package goes.

## For Length-ID == 0
Have total length of bits to recurse on, don't know separator. Need to parse next bytes and see where end is.
Key: While last-bit-parsed < expected length then start parsing from last-bit-parsed. Take result and append to `subpackets` field.

## For Length-ID == 1
Only know how many packets at this level. Don't know last bit. So maybe parse next packet and it knows it's end?
Key: While packets parsed < expected # parsed then continue parsing from last bit of last packet parsed. Take results and append to `subpackets` field.


## If packet is a literal than we're all done!
