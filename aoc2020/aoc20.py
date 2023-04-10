import sys
import re
import json

def revstr(string):
    return ''.join(reversed(string))

def roll(lst):
    return [lst[-1]] + lst[:-1]

def fliptiledata(tiledata):
    return re.sub('([.#]*)', lambda m: revstr(m.group(1)), tiledata)

def paintmonster(working, startspot):
    seamonsterpat = ('                  # \n'
                     '#    ##    ##    ###\n'
                     ' #  #  #  #  #  #   ')
    lines = working.split('\n')
    width = len(lines[0])
    for rowidx, row in enumerate(seamonsterpat.split('\n')):
        for colidx, ch in enumerate(row):
            if ch == ' ':
                continue
            workspot = startspot + colidx + (width + 1) * rowidx
            assert working[workspot] == '#', f"{startspot} {colidx} {rowidx} {width}"
            working = working[:workspot] + 'O' + working[workspot+1:]
    return working

def rolltiledata(tiledata):
    # left becomes top, top becomes right, etc
    coords = {}
    maxrow = None
    for rowidx, row in enumerate(tiledata.split('\n')):
        maxrow = rowidx
        for colidx, ch in enumerate(row):
            coords[rowidx + 1j *colidx] = ch
    newcoords = {}
    for coord, val in coords.items():
        newcoords[(coord - maxrow) * (-1j)] = val
    newval = []
    for rowidx in range(maxrow+1):
        row = ''
        for colidx in range(maxrow+1):
            row += newcoords[rowidx + 1j *colidx]
        newval.append(row)
    return '\n'.join(newval)

def stitch(tilerec, tilecoords):
    # in tilecoords, 1 is up, j is right
    # get the 8-by-8 innermost, stitch
    fullgrid = {}
    for key, tiledata in tilerec.items():
        for rowidx, row in enumerate(tiledata.split('\n')):
            base = 8 * tilecoords[key]
            if rowidx in (0, 9):
                continue
            for colidx, ch in enumerate(row):
                if colidx in (0, 9):
                    continue
                fullgrid[base - rowidx + 1j *colidx] = ch
    minreal = min(int(c.real) for c in fullgrid)
    maxreal = max(int(c.real) for c in fullgrid)
    minimag = min(int(c.imag) for c in fullgrid)
    maximag = max(int(c.imag) for c in fullgrid)
    retval = ''
    # yeah, putting it together upside-down, sort of. That's fine.
    for myreal in range(minreal, maxreal+1):
        for myimag in range(minimag, maximag+1):
            retval += fullgrid[myreal + 1j*myimag]
        retval += '\n'
    return retval.strip()

def doit():
    with open('aoc20.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)

    tilesdata = '\n'.join(data).split('\n\n')
    tiles = {}
    tilerec = {}
    for tiledata in tilesdata:
        m = re.match(r'Tile (\d+):\n([.#\n]*)$', tiledata)
        tilenum = int(m.group(1))
        tilerec[tilenum] = m.group(2)
        tsigm1 = re.match('([#.]*)', m.group(2))
        tsig1 = tsigm1.group(1)
        tsig2 = ''.join(re.findall(r'([.#])$', m.group(2), re.MULTILINE))
        tsig3 = ''.join(reversed(re.search(r'([.#]+)$', m.group(2)).group(1)))
        tsig4 = ''.join(reversed(re.findall(r'^([.#])', m.group(2), re.MULTILINE)))
        tiles[tilenum] = [tsig1, tsig2, tsig3, tsig4]

    print(json.dumps(dict((tilek, tiles[tilek]) for tilek in (1013,2221,3673,1373)), sort_keys=True, indent=4))

    # now flip tiles until all are oriented the same way
    knownset = set([min(tiles)])
    while len(knownset) < len(tiles):
        knownsigs = set(sig for known in knownset for sig in tiles[known])
        tilescopy = dict(tiles)
        for (key, value) in tilescopy.items():
            if key in knownset:
                continue
            if any(revstr(sig) in knownsigs for sig in value):
                knownset.add(key)
                continue
            if any(sig in knownsigs for sig in value):
                # reverse tile, then add to knownsigs
                tiles[key] = [revstr(value[sign]) for sign in (0, 3, 2, 1)]
                knownset.add(key)
                tilerec[key] = fliptiledata(tilerec[key])
                continue
    # allsigs = [sig for val in tiles.values() for sig in val]
    # print(json.dumps(list(sorted(allsigs)), sort_keys=True, indent=4))
    # return

    print(json.dumps(dict((tilek, tiles[tilek]) for tilek in (1013,2221,3673,1373)), sort_keys=True, indent=4))

    
    tilecoords = {}
    tilecoords[min(tiles)] = 0 + 0j
    while len(tilecoords) < len(tiles):
        tilescopy = dict(tiles)
        for (key, value) in tilescopy.items():
            if key in tilecoords:
                continue
            foundmatchtile = None
            for testidx in (0, 1, 2, 3):
                testsig = value[testidx]
                for testtile in tilecoords:
                    if revstr(testsig) in tiles[testtile]:
                        foundmatchtile = testtile
                        break
                else:
                    continue
                matchval = tiles[foundmatchtile]
                while not any(matchval[(i+2)%4] == revstr(value[i]) for i in range(4)):
                    value = roll(value)
                    tilerec[key] = rolltiledata(tilerec[key])

                tiles[key] = value
                matchdir = [i for i in range(4) if revstr(matchval[i]) in value]
                assert len(matchdir) == 1
                tilecoords[key] = tilecoords[foundmatchtile] + (1j ** matchdir[0])
                if len(tilecoords) != len(set(tilecoords.values())):
                    print(tilecoords)
                    print(json.dumps(dict((tilek, tiles[tilek]) for tilek in tilecoords), sort_keys=True, indent=4))
                    raise Exception()
                break

    minreal = min(int(c.real) for c in tilecoords.values())
    maxreal = max(int(c.real) for c in tilecoords.values())
    minimag = min(int(c.imag) for c in tilecoords.values())
    maximag = max(int(c.imag) for c in tilecoords.values())
    # print(sorted(repr(coord) for coord in tilecoords.values()))
    corners = [minreal + 1j*maximag, minreal + 1j*minimag,
               maxreal + 1j*maximag, maxreal + 1j*minimag]
    tilegoals = [tile for tile in tiles if tilecoords[tile] in corners]
    print(tilegoals)
    print(tilegoals[0]*tilegoals[1]*tilegoals[2]*tilegoals[3])

    stitched = stitch(tilerec, tilecoords)
    seamonsterre = \
        re.compile('|'.join(
            r'(?:(?<=^.{%d})..................#..*\n'
            r'^.{%d}#....##....##....###.*\n'
            r'^.{%d}.#..#..#..#..#..#...)' % (x,x,x) for x in range(144)),
                   flags=re.MULTILINE)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = fliptiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        stitched = rolltiledata(stitched)
    if not re.search(seamonsterre, stitched):
        raise Exception()
    seamonstercount = 0
    working = stitched
    m = re.search(seamonsterre, working)
    while m:
        working = paintmonster(working, m.start(0))
        seamonstercount += 1
        m = re.search(seamonsterre, working)

    print(working)
    print(seamonstercount)
    print(len(re.findall('#', stitched)) -
          15*seamonstercount)
    print(len(re.findall('#', working)))

if __name__ == '__main__':
    doit()
