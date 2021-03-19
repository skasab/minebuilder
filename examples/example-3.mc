define boulder as
{
    .C.
    CCC
    .C.
    -
    CCC
    CCC
    CCC
    -
    .C.
    CCC
    .C.
    -
}

define house as
{
    load house.mpb
}

repeat boulder starting at (48,85,-384) for 20 times, 3 apart in y direction
repeat house starting at (55,85,-384) for 20 times, 10 apart in x direction
end
