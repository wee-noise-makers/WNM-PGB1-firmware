#!/usr/bin/env python3

# This script generates chords definitions in roman numeral notations as well
# lists of chord progressions.

import re
import roman

# The following chord progressions are comming from the
# https://github.com/ldrolez/free-midi-chords repository

###############################################################################
# Copyright (c) 2019-2023 Ludovic Drolez

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Major progressions
prog_maj = [
    #"I I IM-5 IM-5 IV IV V Vsus2",
    "I I IV iii",
    "I iii IV vi",
    "I iii vi Isus4",
    "I iii vi IV",
    "I IV ii V",
    "I IV Isus2 IV",
    "I IV V IV",
    "I IV V V",
    "I IV vi V",
    "I IV vii iii vi ii V I",
    "I V I IV",
    "I V IV vi",
    "I V vi ii",
    "I V vi iii IV I IV V", 
    "I V vi iii IV",
    "I V vi IV",
    "I V vi V",
    "I vi I IV", 
    "I vi ii IV", 
    "I vi ii V", 
    "I vi IV iii",
    "I vi IV V",
    "I7 V7 viadd9 IV7",
    "ii IV V V",
    "ii IV vi V",
    "ii V I I",
    "ii V I IV",
    "ii7 Vadd9 I7 I7",
    "iii vi IV I",
    "iim7 V7 iiim7 vi7 iim7 V7",
    "Isus2 I vi7 visus4",
    "IV I ii vi",
    "IV I iii IV",
    "IV I V vi",
    "IV IV I V",
    "IV vi I V",
    "IV vi iii I",
    "IV vi IV vi",
    "V I vi V",
    "V IV vi I",
    "V vi IV I",
    "vi ii V I",
    "vi IV I V",
    "vi V IV V ii V I I",
    "vi V IV V",
]

# minor progressions
prog_min = [
    # "i i iv iv v7 ii5 v v7",
    "i ii v i",
    "i III iv VI",
    "i III VII VI",
    "i ii v III i ii v VII",
    "i iv III VI",
    "i iv v iv",
    "i iv v v",
    "i iv VI v",
    "i iv VII i",
    "i iv VII v i i ii V",
    "i v iv VII",
    "i vdim iv VI",
    "i VI III VII i VI69 III7 VII",
    "i VI III VII",
    "i VI iv ii",
    "i VI iv III",
    "i VI iv v",
    "i VI VII iv",
    "i VI VII v",
    "i VI VII VII",
    "i VII i v III VII i v i",
    "i VII i v",
    "i VII III VI",
    "i VII v VI",
    "i VII VI III iv VI VII i",
    "i VII VI III",
    "i VII VI iv",
    "i VII VI VII",
    "i7 VI III7 VII6 i i7 III7 iv7",
    "ii v i i",
    "ii v i iv",
    "ii VI i iv",
    "im7 ivsus4 v7 isus4",
    "iv i v VI",
    "iv III VII i",
    "iv III vsus4 VI iv i III VI",
    "iv v VI VII",
    "iv VI v VII",
    "iv VI VII i",
    "v i iv VII",
    "v iv i i",
    "v VI III i",
    "v VI v i",
    "VI i v III",
    "VI i v v",
    "VI III i v",
    "VI iv i v",
    "VI VI i VII",
    "VI VII i III",
    "VI VII v III",
    "VII iv v i",
    "VII iv VII i",
]

# No. Mode
# 1   Ionian (major)  I   ii  iii   IV   V   vi  viio
# 2   Dorian          i   ii  ♭III  IV   v   vio ♭VII
# 3   Phrygian        i   ♭II ♭III  iv   vo  ♭VI ♭vii
# 4   Lydian          I   II  iii   ♯ivo V   vi  vii
# 5   Mixolydian      I   ii  iiio  IV   v   vi  ♭VII
# 6   Aeolian (n.min) i   iio ♭III  iv   v   ♭VI ♭VII
# 7   Locrian         io  ♭II ♭iii  iv   ♭V  ♭VI ♭vii
# Modal progressions
prog_modal = [
    "bIIIM ii bIIM I",
    "bIIM bVIM biii bviim",
    "bIIM ivm biii im",
    "bVIIM bIIM bIIIM im",
    "bVIM bIIIM bVIIM IV I",
    "bVIM bVIIm im bIIM",
    "bVIM vi im bVIIM",
    "bVIM7 ivmadd9 I I",
    "I bIIIM bVIIM I",
    "I bIIIM bVIIM IV",
    "I bIIIM bVIM bVIIM",
    "I bIIIM IV I",
    "I bIIM I iii",
    "I bIIM7 bIIIM6 bIIM7 I im bVIIM bIIM",
    "I bVIIM bVIM bIIM",
    "I bVIIM bVIM IV IVsus4 IV",
    "I bVIIM I I bVIM V",
    "I bVIIM IV V", 
    "I bVIM I bIIM",
    "I bVIM IV bIIIM bVIIM",
    "I IIIM vi V",
    "I IIM iii V6",
    "I IIM IV I",
    "I IV bIIIM bVIM", 
    "I IV bVIIM IV",
    "I IV V bVIIM",
    "I ivm bIIIM bVIIM",
    "I V bVIIM IV",
    "I V ivm bVIM",
    "I5 iii II5 #IVm IV5 vi V5 viim",
    "ii bIIM I bVIIM",
    "ii bVIIM7 I", 
    "ii IVM vm bVIIM",
    "IIIM V VIsus4 VIM I IIM",
    "im bIIIM bVIIM IV",
    "im bIIIM bVIM V",
    "im bIIIM IV bVIM",
    "im bIIM bIIIM bIIM",
    "im bIIM biim6 ivm",
    "im bIIM im7 bviim",
    "im bIIM ivm IIIM bIIM ivm IIIM IIIM",
    "im bIIM vm im7",
    "im bVIIM bIIM vm",
    "im bviim bVIM bIIM",
    "im bVIM ivm V",
    "im ii vm IV",
    "im ivm9 bIIM im vm ivm7 bIIM im7",
    "im ivm9 bIIM im",
    "im V bVIIM IV bVIM bIIIM ivm V",
    "im VIM bi V",
    "im VIM IIIM bIIM",
    "im vm bVIM bIIM",
    "im vm ivm bIIM7",
    "IV V ii im bIIIM IV",
    "ivm bIIIM iim7 V",
    "ivm im bviim bVIM",
    "vdim vdim ivm bIIIM",
    "vi bVIM bVIIM I",
    "vi vii V vi #IVdim V",
    "VIM bVIM im bVIIM",

    # cadences
    "bIIIM V7 I",
    "bVIIM V7 I",
    "im bVIIM IV im",
    "ivm bIIIM bIIM I",
    "ivm IIIM bIIM I",
    "ivm bIIIM bVIM I",
]

###############################################################################

p = re.compile("(?:(?P<flat>b)|(?P<sharp>#))?(?:(?P<minor>[vi]+)|(?P<major>[VI]+))(?P<func>sus2|sus4|dim|add9|7|m7|5|69|m|M)?")

accidents = {
             "b": "Flat",
             "": "Natural",
             "s": "Sharp",
             }

# Conversion table for WNM chord function names
functions = {
             "min": "Min_Triad",
             "maj": "Maj_Triad",
             "dim": "Dim_Triad",
             "sus4": "Sus4",
             "sus2": "Sus2",
             "7": "Maj_7th",
             "m7": "Min_7th",
             "5": "Five",
             "m5": "Min_Five",
             "6add9": "Six_Add_Nine"
            }

# First print a definition for all the chords in roman numeral notation
for kind in ["min", "maj", "dim", "sus4", "sus2", "7", "m7", "5"]:
    print (f"   --  {kind}")
    for accident in ["b", "", "s"]:
        for degree in range(1, 8):
            name = accident + roman.toRoman(degree) + kind
            accident_name = accidents[accident]
            function = functions[kind]
            print(f"   {name} : constant Roman_Numeral_Notation "
                  f":= ({degree}, {accident_name}, {function});")

class Chord():
    def __init__(self, name):
        self._match = p.match(name)

    @property
    def valid(self):
        return self._match is not None

    @property
    def name(self):
        groups = self._match.groupdict()
        # print(groups)

        accident = ""
        if groups["flat"]:
           accident = "b" 
        elif groups["sharp"]:
           accident = "s" 

        kind = ""
        if groups["minor"]:
            degree = roman.fromRoman(groups["minor"].upper())
            kind = "min"
        elif groups["major"]:
            degree = roman.fromRoman(groups["major"])
            kind = "maj"
        else:
            raise

        if groups["func"]:
            if kind == "maj":
                func_to_kind = {
                    "sus2": "sus2",
                    "sus4": "sus4",
                    "dim": "dim",
                    "add9": "add9",
                    "7":"7",
                    "m7":"m7",
                    "5": "5",
                    "m5":"m5",
                    "69": "6add9",
                    "m": "min",
                    "M": "maj"}
            elif kind == "min":
                func_to_kind = {
                    "sus2": "sus2",
                    "sus4": "sus4",
                    "dim": "dim",
                    "add9": "add9",
                    "7":"m7",
                    "m7":"m7",
                    "5": "m5",
                    "m5":"m5",
                    "69": "6add9",
                    "m": "min",
                    "M": "maj"}
            kind = func_to_kind[groups["func"]]

        return accident + roman.toRoman(degree) + kind

def convert_progressions (name, progessions):
    id = 0
    for prog in progessions:

        # Filter chords that we don't support
        if "add9" in prog or "69" in prog:
            continue

        list = []
        for elt in prog.split(" "):
            C = Chord(elt)
            # print(f"{elt} -> {C.name}")
            list.append(C.name);

        print(f"   {name}_{id} : aliased constant Roman_Numeral_Progression := \n"
               "     (" + ", ".join(list) + ");\n"
               "   --  " + prog + "\n")
        id = id + 1

    print(f"   {name}s : aliased constant Progression_Collection := (")
    for x in range(id):
        coma = "," if x != (id - 1) else ""
        print(f"     {name}_{x}'Access{coma}")
    print("     );")

convert_progressions("Major_Progression", prog_maj)
convert_progressions("Minor_Progression", prog_min)
convert_progressions("Modal_Progression", prog_modal)
