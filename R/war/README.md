# Fantasy WAR

[Source](https://www.fantasypoints.com/nfl/articles/season/2021/fantasy-war-part-1-theory#/)

Who was the best fantasy football player of the year? Was it the player
who scored the most points by any player at any position? Was it the the
early first-round pick who led all RBs? What about the player, who
outscored the \#2 of his position by over 50 points? You might think
there is no way to tell for sure and “to each their own.” Think again.

I’m here to introduce a new statistic called Wins Above Replacement
(WAR) for fantasy football, and it directly answers this question. WAR
is a fine-tuned tool capable of controlling for everything imaginable
that reveals the estimated number of wins each and every player provided
the average fantasy team in a given year.

WAR was built to power redraft managers on draft day. When you sit down
to make each selection this August, you should have one question on your
mind: “Who is going to give my fantasy team the most wins?” That’s it.
Nothing else matters. Of course, there’s no way to tell for sure which
specific player will provide the most wins for your team. But with WAR,
you’ll have the almanac at your hands revealing what the optimal draft
looked like for 2020. This in turn will help you make key decisions for
your 2021 selections.

In this introduction, a thorough explanation of this model will be
explored in the following three parts:

1.  Wins Above Replacement Theory
2.  Positional Hierarchy
3.  Year-Over-Year WAR Trends

## Part 1: Wins Above Replacement Theory

WAR was originally created as a baseball statistic in the 1980s to
estimate the number of wins a baseball player provided his team in a
given year by controlling for everything else around him. The core
concept is the same for fantasy football. WAR will tell fantasy managers
how many more head-to-head matchups they should expect to win in a
season from starting a particular player vs. that player’s expected
replacement-level fill-in.

In order to make this happen, specific information on the following will
need to be established in order to convert this baseball sabermetric to
the fantasy football world:

1.  League Settings
2.  Average weekly team score
3.  Average team standard deviation
4.  “Replacement Level” Definition
5.  Player of focus

### \#1 League Settings

**Adapted to RFL**

The starting lineup consist of QB, 2 RB, 2 WR, TE, 2 Flex, PK, 2 DL, 2
LB, 2 DB, 3 IDP.

We have 36 Teams in the league, but 3 copies per player. So, we handle
the league basically as a 12-team league.

Let’s load our base data.

    head(starter, 5)

    ## # A tibble: 5 × 10
    ##   season  week franchise_id starter_status player_id player_name     pos   team 
    ##    <dbl> <dbl> <chr>        <chr>              <dbl> <chr>           <chr> <chr>
    ## 1   2022     1 0016         starter            13593 Jackson, Lamar  QB    BAL  
    ## 2   2022     1 0016         starter            12630 Drake, Kenyan   RB    BAL  
    ## 3   2022     1 0016         starter            15256 Williams, Javo… RB    DEN  
    ## 4   2022     1 0016         starter            14835 Higgins, Tee    WR    CIN  
    ## 5   2022     1 0016         starter            10271 Jones, Julio    WR    TBB  
    ## # ℹ 2 more variables: player_score <dbl>, should_start <dbl>

### \#2: Average Team Weekly Score

**Adapted to RFL**

The average team weekly score is calculated by adding the individual
scores of all starting positions together. In order to assign an average
score for each starting roster spot, we’ll use the average points from
the top-x started players of each regular season week.

<table>
<thead>
<tr class="header">
<th>Position</th>
<th>Top-x</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>QB</td>
<td>12</td>
</tr>
<tr class="even">
<td>RB</td>
<td>24</td>
</tr>
<tr class="odd">
<td>WR</td>
<td>24</td>
</tr>
<tr class="even">
<td>TE</td>
<td>12</td>
</tr>
<tr class="odd">
<td>PK</td>
<td>12</td>
</tr>
<tr class="even">
<td>DL</td>
<td>24</td>
</tr>
<tr class="odd">
<td>LB</td>
<td>24</td>
</tr>
<tr class="even">
<td>DB</td>
<td>24</td>
</tr>
</tbody>
</table>

Take QB for example. The league has 12 starting QBs per week and 13
total weeks in a season. This means there are 156 starting QB outputs.
The 12 most-started QBs in each week make up these 156 performances,
which are simply averaged to provide the expected fantasy points from an
average starting QB.

Since we are playing with 3 player copies, the amount of started player
can be between 0 (nobody hast started him) and 3 (all three teams
started him). In later calculations we include this count to reflect the
as starter considered players.

    head(eligable_players, 5)

    ## # A tibble: 5 × 10
    ##   season  week player_id pos   start_pct player_score  rank eligable flex 
    ##    <dbl> <dbl>     <dbl> <chr>     <dbl>        <dbl> <int>    <dbl> <chr>
    ## 1   2022     9     13131 RB            1         55.1     1        1 <NA> 
    ## 2   2022    12     14073 RB            1         48.3     1        1 <NA> 
    ## 3   2022     2     12186 WR            1         44.8     1        1 <NA> 
    ## 4   2022     8     13132 RB            1         42.8     1        1 <NA> 
    ## 5   2022     2     13593 QB            1         42.6     1        1 <NA> 
    ## # ℹ 1 more variable: rank_flex <int>

This process is repeated for all other positions. Since some of them
require two starting slots, the average points from the top-24 started
per week is used. The process is the same for flex and IDP as a single
starting position, except that all players considered a starter in their
native position are not eligible for flex, as it’s presumed these
players already occupy a starting spot somewhere in the league. However,
the next top-24 most-started combination of RB, WR, and TE for offense
and DL, LB and DB for defense each week do qualify.

Simply adding all positional averages from this process together
produces the expected amount of points the average team in a 12-team
half-point PPR league for any given week.

For 2022, that turned out to be 234.58.

### \#3 Average Team Standard Deviation

The process of determining the average points produced from each
individual starting roster spot is performed the same for standard
deviation. The only difference is that the standard deviation is
calculated on these positions instead of the average. But the process
for determining the team-level standard deviation is more complex and
requires each individual position’s standard deviation plugged into the
following formula:

sd\_avg\_team = √(∑(sd\_i^2))

i = positions \* starter

To briefly explain the equation above, each individual starting
position’s standard deviation is squared and added together. The square
root of the resulting figure provides the team-level standard deviation
of the average team. For the average team in 2022, the standard
deviation turned out to be 29.47.

In plain terms, this means that about 68% of fantasy team weekly scores
fell between 29.47 points below or above the league average of 234.58.
The idea that about 70 percent of weekly team scores in the league were
between 205.11 and 264.05 seems about right. And if stretched to two
standard deviations, 95 percent of team scores fell between 175.65 and
293.51

Why does knowing team-level standard deviation matter for Wins Above
Replacement? In order to ultimately estimate the likelihood that any
individual player will lead your team to win a head-to-head matchup, the
distribution of possible outcomes expected from your opponent must be
known. We’ll come back to this soon.

### \#4 Replacement Level Definition

The most critical piece of the WAR puzzle is to determine how valuable a
replacement-level player at each position is. But first, we need to
figure out what replacement exactly means. The idea of a replacement
player is that if you have a starting player miss a game due to injury,
suspension, bye, etc., you are forced to insert your next best option
into your starting lineup. This would either be someone on your bench or
from the waiver wire/free agent pool.

Let’s draw this out with QB. In a 12-team league, WAR assumes the top-12
league started QBs are in one of the twelve team’s lineups.

Let’s say your QB is on bye and you either have to either stream or use
a backup. The question is: how many points can you expect out of the
“next-best” QB? To figure this out, we need to find the average of the
next top-12 started QBs after all determined league starting QBs for
each week. We use the top-12 per week because we can’t assume our team
will have the best replacement-QB in our league or the worst, so we use
the average. All in all, 156 performances (12 per week \* 13 weeks) for
replacement-level QBs are averaged revealing the estimated points from a
replacement-QB.

The process is the same for all positions, with the exception that twice
the amount of starters are removed from consideration for RB and WR, due
to the fact that each has two starting roster spots. From there, each
position has a figure for what is estimated out of a replacement-level
player. Here is what each position’s expected weekly points out of a
replacement turned out to be in 2020:

    ## # A tibble: 8 × 2
    ##   pos   points_replacement_player
    ##   <chr>                     <dbl>
    ## 1 DB                        12.0 
    ## 2 DL                         9.76
    ## 3 LB                         8.74
    ## 4 PK                         7.38
    ## 5 QB                        17.3 
    ## 6 RB                         7.79
    ## 7 TE                         9.30
    ## 8 WR                        11.4

These figures will be used at the end of the next section to compare any
player against at his position as the final step in the WAR process.

### \#5 Player of Focus

Now comes the fun part. We are ready to use all this information to
calculate a player’s WAR. The idea is to figure out the odds the average
fantasy team has of winning each matchup when all that is known are the
points the player of focus (POF) provided throughout the year.

Example: In Week 1, the POF scored 20 fantasy points, which happened to
be 4.4 points more than an average starting RB’s 15.6. Let’s say, our
POF is a RB. So the rest of the starting positions on our team are
expected to perform to league average as earlier explained. With the
average expected points of all other starting roster spots on our team
added together along with the POF’s 20 points, the POF owner would
expect to have scored 238.98 points in Week 1 without knowing anything
about any other player.

Now we need to know what to expect out of the Kamara owner’s opponent.
This is done by adding the average contribution of each position to
obtain the expected team output: 234.58.

Here’s what this all looks like on paper so far:

#### Week 1 Examples

    ## # A tibble: 16 × 3
    ##    position opponent `POF Team`
    ##    <chr>       <dbl>      <dbl>
    ##  1 QB          20.5       20.5 
    ##  2 RB          15.6       15.6 
    ##  3 RB          15.6       20   
    ##  4 WR          15.8       15.8 
    ##  5 WR          15.8       15.8 
    ##  6 TE          10.5       10.5 
    ##  7 FLEX        11.4       11.4 
    ##  8 FLEX        11.4       11.4 
    ##  9 PK           9.01       9.01
    ## 10 DL           9.18       9.18
    ## 11 DL           9.18       9.18
    ## 12 LB          16.7       16.7 
    ## 13 LB          16.7       16.7 
    ## 14 DB          12.9       12.9 
    ## 15 DB          12.9       12.9 
    ## 16 TOTAL      235.       239.

Notice that all expected scores are the same for each position on both
teams with the only exception being the POF’s 20. This gives the POF
owner an expected advantage over the average fantasy opponent.

There’s one problem here. We can’t just assume with 100% certainty that
the POF led our team to victory in Week 1 simply due to the higher
projected team total. This is where the team-level weekly standard
deviation formula comes into play. Maybe our opponent does score exactly
234.58 points; but maybe they score 300 – or only 180. As any fantasy
player knows, it’s going to vary week to week.

To account for this variance, a cumulative distribution function takes
the average opponent expected score (`r 234.58`) and the average team
standard deviation (\`r 29.4657882) and determines the likelihood of a
score in this presumed normal distribution being below our team’s
expected score of 238.98. And for the POF in Week 1, this turned out to
be 64%. Simply put: Without knowing the results of any other player than
Alvin Kamara in a head-to-head matchup for Week 1, Fantasy players had
an estimated 64% chance of winning the matchup after scoring 20 points.

The same process is repeated for all regular season weeks which provides
estimated win percentages for all matchups. For all games the POF missed
(bye, injury, suspension), the expected output of a replacement-level
Player is used to fill in his stead. Once all expected fantasy team win
percentages are calculated for the POF owner, the seasonal fantasy team
win percentage is simply the average of all weeks.

Now we know the expect win percentage solely due to the contribution of
the POV. This is great and all, but we still need to figure out how much
better this is than a replacement-level player. So we calculate a win
percentage fo rthe replacement level player the same way we did for the
POF. Last but not least, we substract the expected wins of the
replacement level player from the POF’s and the result: WAR.

Since we are playing double header, we multiply the WAR by 2.

And that is it. That is the full process to complete a fantasy football
player’s WAR. Everything is simply repeated for every player until all
relevant names are crunched and a full list of the most important
players emerge.

### Results 2022

    ## # A tibble: 776 × 6
    ##    season player_id player_name          pos   points   war
    ##     <dbl>     <dbl> <chr>                <chr>  <dbl> <dbl>
    ##  1   2022     13699 Evans, Rashaan       LB      227   3.04
    ##  2   2022     13319 Jones, Aaron         RB      202.  2.7 
    ##  3   2022     13404 Ekeler, Austin       RB      270.  2.56
    ##  4   2022     14073 Jacobs, Josh         RB      264.  2.48
    ##  5   2022     13130 McCaffrey, Christian RB      239.  2.12
    ##  6   2022     14218 Okereke, Bobby       LB      191   2.06
    ##  7   2022     11244 Kelce, Travis        TE      247.  1.96
    ##  8   2022     12186 Diggs, Stefon        WR      271.  1.96
    ##  9   2022     11675 Adams, Davante       WR      268.  1.92
    ## 10   2022     14892 Brooks, Jordyn       LB      237   1.92
    ## # ℹ 766 more rows
