###############
#	labeler.praat
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
word Word
word Vowel
endform

begin = Get start of selection
end = Get end of selection

#Move cursor to... end+1
Move cursor to... end
Add interval on tier 1
Add interval on tier 2

#Move cursor to... begin+1
Move cursor to... begin
Add interval on tier 1
Add interval on tier 2

Select... begin end
Extract selected sound (preserve times)

endeditor

Rename... jawn
To Formant (burg)... 0.0 5 5500 0.025 50


maxf1 = Get time of maximum... 1 0 0 Hertz Parabolic
maxf2 = Get time of maximum... 2 0 0 Hertz Parabolic

select Sound jawn
Remove
select Formant jawn
Remove
select TextGrid 'tgrid$'

t = (end-begin)/2
it = begin+t

int = Get interval at time... 1 it
lint = Get number of intervals... 1
lint = (lint-1)/2
Set interval text... 1 int 'Word$''lint'
Set interval text... 2 int 'Vowel$'

editor
Select... maxf1 maxf2






