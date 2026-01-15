define x = 3; Global variable

match tag {
    0x4c495354
    0x44494354
    0x5345545f
    0x5455504c -> load64(x + 8)
    0x44494354
    0x5345545f
    0x5455504c -> load64(x + 8)
    otherwise
      -> runsomething()
}

match tag {0x4c495354 0x44494354 0x5345545f 0x5455504c -> load64(x + 8) 0x44494354 0x5345545f 0x5455504c -> load64(x + 8) otherwise -> runsomething()}

