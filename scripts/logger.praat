###############
#	logger.praat
#	Copyright 2009 Josef Fruehwald
#    This file is part of Vowel Logger.
#
#    Vowel Logger is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Vowel Logger is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Vowel Logger.  If not, see <http://www.gnu.org/licenses/>.
###############

form Test
## Change 'name' below to the name of the text grid being annotated to
word tgrid muffet
## change 'log.txt' to wherever you want the logged values to go to
word output ~/log.txt
real NucleusWindow 0.005
real GlideWindow 0.002
endform

begin = Get start of selection
end = Get end of selection

endeditor
select TextGrid 'tgrid$'
int = Get interval at time... 1 begin
ibegin = Get start point... 1 int
iend = Get end point... 1 int

word$ = Get label of interval... 1 int
vowel$ = Get label of interval... 2 int

editor

dur = end - begin
idur = iend - ibegin



Move cursor to... begin
Add interval on tier 3

endeditor
select TextGrid 'tgrid$'
pbegin = Get nearest index from time... 3 begin
Set point text... 3 pbegin nuc
editor


Select... begin+'NucleusWindow' begin-'NucleusWindow'
nf1 = Get first formant
nf2 = Get second formant
nf3 = Get third formant

if begin = end
	gf1 = nf1
	gf2 = nf2
	gf3 = nf3
	hasglide$ = "NoGlide"
else
	hasglide$ = "Glide"
	Move cursor to... end
	Add interval on tier 3

	endeditor
	select TextGrid 'tgrid$'
	pend = Get nearest index from time... 3 end
	Set point text... 3 pend glide
	editor

	Select... end+'GlideWindow' end-'GlideWindow'
	gf1 = Get first formant
	gf2 = Get second formant
	gf3 = Get third formant
endif


fileappend 'output$' 'word$''tab$''vowel$''tab$''ibegin:5''tab$''iend:5''tab$''idur:5''tab$''begin:5''tab$''end:5''tab$''dur:5''tab$''nf1:0''tab$''nf2:0''tab$''nf3:0''tab$''gf1:0''tab$''gf2:0''tab$''gf3:0''tab$''hasglide$''newline$'