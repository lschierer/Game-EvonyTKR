520 in the formulas below is the maximum amount of cultivation possible. its a reasonable constant. 

44 in the formulas below is a general's level.  That's actually a variable.  Its also not a reasonable value for most players.  A better default would be 40.  Values can go from 1 to 50.  ultimately, just as I allow users to pick ascending levels, covenant levels, and specialty levels, this should be a toggle, although while the *theoretical* max range is 1 to 50, I doubt there's any need to display the *really really* low values, so my intent is to set it to display about 25-50.  

some of the rest is because, as best everyone can figure out, the relationship between the basic attributes and buffs are:
* For stats below 900, the buff will be 0.1% per stats.
* The portion exceeding 900 will be buffed 0.2% per stats.

so you take .1% of the first 900, and add that to .2% of anything above that. 

I'm honestly not sure though where The EvonyAnswers author got the 2.4867 from though.  Many of his calculations used seemingly arbitrary scaling factors that he felt made calculations more accurate.  In this case though, I've observed that when I increase different generals levels, the results I get *do* in fact have some extra scaling factor.
* Moving Romulus from level 27 to level 28, I go from 1289 leadership, 1269 attack, 1253 defense and 1190 politics to 1314 leadership, 1293 attack, 1277 defense, 1212 politics.  That's well over his published 'increment' values.  Some of this comes from my victory column, which "Increases Generals' basic attributes +9%" at level 9.   I'm unsure if that's enough to account for the difference?  If not, that might be what he's doing with the 2.4867.  He might be attempting to average out the deltas he's seen between observed increases and published increments.   

## Attack
the Attack basic attribute only affects the Attack buff.

=ROUND(((900*0.1)+(((L8+(M8*2.4867*44))*1.1+50+520)-900)*0.2)/100,3)
L8 is the cell that contains the 'base' attribute.
M8 is the cell that contains the 'increment' attribute.

## Defense
The Defense basic attribute only affects the Defensse buff.

=ROUND(((900*0.1)+(((X8+(Y8*2.4867*44))*1.1+50+520)-900)*0.2)/100,3)
X8 is the cell that contains the 'base' attribute.
Y8 is the cell that contains the 'increment' attribute.

## HP
There is no HP basic attribute.  

=ROUND(((900*0.1)+(((AJ8+(AK8*2.4867*44))*1.1+50+520)-900)*0.2)/100,3)
AJ8 is the cell that contains the 'base' attribute.
AK8 is the cell that contains the 'increment' attribute.

## Leadership
The Leadership basic attribute affects:
- Subordinate City Training Speed When this general is the Mayor
- Troop HP
- March Speed

Thus use this attribute for the HP formula above.

## Politics
The Politics basic attribute affects:
- Subordinate City Construction Speed When this general is the Mayor
- Subordinate City Gold Production Speed When this general is the Mayor
- Resource Gathering Speed
- Troop Death to Wounded

Since we do not currently use Troop Death to Wounded, we ignore this.
