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
# Legend
X - done
I - in progress
_ - todo
P - paused
S - skipped

# TODOs
* [X] read literal outputs struct
* [X] string->packge should take start position
* [P] test for embedded packets
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

# Last-bit-position to all packets
Problem: when parsing the bits we don't always know the total length of bits to consider when parsing packets. We cannot just assume from the current bit to the end. Here is a case that illustrates that example. Packet A contains packet B and C. Packet B contains packets D and E. That means that when parsing B we'll need to parse D and E. For the parsing function we cannot just say "from the start of D to the end of all bits" since that would include packet C. 

Proposed Solution: We'll lean on the confidence we place in recursive functions returning what we expect. Each packet will contain a `last-bit-pos` which determines the position of the last bit in the packet.

This makes things interesting when considering that there can be bits thrown away at the end of a sequence. Not really sure how to handle this problem but lets make the issue clear here for later reference. Consider parsing the string `38006F45291200`. This comes out to the following binary sequence.

```
00111000000000000110111101000101001010010001001000000000
VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
```

Below each bit is the marker for that bit
V: version
T: type
I: length-ID
L: length
A: sub-packet A
B: sub-packet B

The concern comes from considering if this was a subpacket in something else. Lets call this operator packet, `Packet A`. Say Packet A is embedded in packet B, another operator packet. Next to packet A is packet C, a clone of A. When the code is done parsing A it then needs to know where to start for C. How will it do this?

I'm going to assume that the case outlined here is illegal, packet A would immediately be followed by C with no padding.

Based on the problem statement I suspect that this is the case since only the outer packet can have padding.

```
The BITS transmission contains a single packet at its outermost layer which itself contains many other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at the end; these are not part of the transmission and should be ignored.
```

I'm going to go ahead with this assumption that it's only an issue at the top-level and that everything contained doesn't contain wasted space.
