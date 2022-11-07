;NEMASIM 1.0, Brown, T ., Morgan, E ., Caplat, P. (2022) "USING AGENT BASED MODELS TO INDICATE NEMATODE TRANSMISSION IN HETEROGENOUS LANDSCAPES"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 1: SETUP

extensions [csv]
globals [counter]

patches-own [egg-number
  roe-region
  fallow-region
  strong-barrier
  weak-barrier
  field
  non-field
  meadow]

breed [roes roe]
breed [fallows fallow]

fallows-own [herdmates nearest-neighbor]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 2: START AND RESET MODEL

to setup
  ask patches with [pxcor >= 0 and pxcor <= 112] [set pcolor gray]
  if landscape = "fragmented_strong" [setup-fragmented_strong]
  if landscape = "fragmented_weak" [setup-fragmented_weak]
  if landscape = "fragmented_two_weak" [setup-fragmented_two_weak]
  if landscape = "fragmented_meadows" [setup-fragmented_meadows]
  if landscape = "continuous_strong" [setup-continuous_strong]
  if landscape = "continuous_weak" [setup-continuous_weak]
  if landscape = "continuous_two_weak" [setup-continuous_weak]
  if landscape = "continuous_two_weak" [setup-continuous_two_weak]
  if landscape = "continuous_meadows" [setup-continuous_meadows]
  define-meadow
  define-strong-barriers
  define-weak-barriers
  define-non-field
  define-field
  if ruminant-type = "Roe" [define-roe-regions]
  if ruminant-type = "Roe"[setup-roe-regions]
  if ruminant-type = "Roe" [setup-roes]
  if ruminant-type = "Fallow" [define-fallow-regions]
  if ruminant-type = "Fallow" [setup-fallow-regions]
  if ruminant-type = "Fallow" [setup-fallows]
end

;Start model (button)
to go
   if ruminant-type = "Roe"[
  ask roes [move_roes]
    eggspergram_roe
    if count-down - ticks = 0 [visualise-eggs]
    if count-down - ticks = 0 [stop]]

  if ruminant-type = "Fallow"[
    ask fallows [move_fallows]
  eggspergram_fallow
    if count-down - ticks = 0 [visualise-eggs]
    if count-down - ticks = 0 [stop]]
  tick
 end

;Reset the model (button)
to reset
  print "resetting  model"
  clear-ticks
  clear-turtles
  clear-patches
  clear-drawing
  clear-all-plots
  clear-output
  reset-ticks
  resize-world 0 112 0 112
  ;clear-all ;only us
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 3: DEFINE LOCATIONS WHERE HOSTS CAN MOVES

;Define regions for roe
to define-roe-regions
  ask patches with [pxcor >= 5  and pxcor <= 27 and pycor >= 7 and pycor <= 105] [set roe-region 1]
   ask patches with [pxcor >= 27  and pxcor <= 47  and pycor >= 7 and pycor <= 105] [set roe-region 2]
   ask patches with [pxcor >= 47  and pxcor <= 67 and pycor >= 7 and pycor <= 105] [set roe-region 3]
   ask patches with [pxcor >= 67  and pxcor <= 87  and pycor >= 7 and pycor <= 105] [set roe-region 4]
   ask patches with [pxcor >= 87 and pxcor <= 109  and pycor >= 7 and pycor <= 105] [set roe-region 5]
end

to define-fallow-regions
 ask patches with [pxcor >= 5  and pxcor <= 109 and pycor >= 5 and pycor <= 105] [set fallow-region 1]
end

to define-weak-barriers
  ask patches with [pcolor = blue] [set weak-barrier true]
end

to define-strong-barriers
  ask patches with [pcolor = black] [set strong-barrier true]
end

to define-non-field
  ask patches with [pcolor = gray] [set non-field true]
end

to define-field
  ask patches with [pcolor = green] [set field true]
end

to define-meadow
  ask patches with [pcolor = yellow] [set meadow true]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 4: SETUP WILD ROE REGIONS AND SPAWN DEER

to setup-roe-regions
    if any? patches with [roe-region = 1 ] and wild_pop >= 1 [
    ask one-of patches with [roe-region = 1 and non-field = true] [sprout-roes 1]]

  if any? patches with [roe-region = 2] and wild_pop >= 2 [
    ask one-of patches with [roe-region = 2 and non-field = true] [sprout-roes 1]]

   if any? patches with [roe-region = 3] and wild_pop >= 3 [
    ask one-of patches with [roe-region = 3 and non-field = true] [sprout-roes 1]]

   if any? patches with [roe-region = 4] and wild_pop >= 4 [
    ask one-of patches with [roe-region = 4 and non-field = true] [sprout-roes 1]]

   if any? patches with [roe-region = 5] and wild_pop >= 5 [
    ask one-of patches with [roe-region = 5 and non-field = true] [sprout-roes 1]]
end

to setup-fallow-regions
  if any? patches with [fallow-region = 1 ] [
   ask one-of patches with [fallow-region = 1 and non-field = true] [sprout-fallows wild_pop]]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 5: SETUP WILD HOST EGGS LARVAE and WORM BURDEN

  to setup-roes
ask roes[
set shape "moose"
    set color black
    set size 5]
end

to setup-fallows
ask fallows[
set shape "moose"
    set color black
    set size 5
    set herdmates no-turtles]
  end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 6: MOVE DEERS

to move_roes
  fd 0.063 fd 0.063 fd 0.063 rt random 10 lt random 10

  ask roes[
    if (random-float 100 > 90) and [strong-barrier] of patch-ahead 0.1 = true ; notice more than symbol
    [fd 0.7]]

  ask roes[
    if (random-float 100 < 90) and [strong-barrier] of patch-ahead 0.1 = true ; notice less than symbol
    [set heading heading - 180]]

  ask roes[
    if (random-float 100 > 50) and [weak-barrier] of patch-ahead 0.1 = true
    [fd 0.7]]

     ask roes[
    if (random-float 100 < 50) and [weak-barrier] of patch-ahead 0.1 = true
    [set heading heading - 180]]

  ask roes[
    if field = true and [non-field] of patch-ahead 3 = true
      [fd 0.7]]

  ask roes with [roe-region = 1][
    if [roe-region] of patch-ahead 1 != 1 [set heading heading - 180 ]]

  ask roes with [roe-region = 2][
    if [roe-region] of patch-ahead 1 != 2 [set heading heading - 180 ]]

      ask roes with [roe-region = 3][
    if [roe-region] of patch-ahead 1 != 3 [set heading heading - 180 ]]

          ask roes with [roe-region = 4][
    if [roe-region] of patch-ahead 1 != 4 [set heading heading - 180 ]]

               ask roes with [roe-region = 5][
    if [roe-region] of patch-ahead 1 != 5 [set heading heading - 180 ]]
end

to move_fallows
fd 0.075 fd 0.075 fd 0.075 rt random 10 lt random 10

  ask fallows[
  if (random-float 100 > 90) and [strong-barrier] of patch-ahead 0.1 = true ; notice more than symbol
    [fd 0.7]]

  ask fallows[
    if (random-float 100 < 90) and [strong-barrier] of patch-ahead 0.1 = true ; notice less than symbol
    [set heading heading - 180]]

  ask fallows[
    if (random-float 100 > 50) and [weak-barrier] of patch-ahead 0.1 = true ;50 refers to impermeablness of fence
    [fd 0.7]]

   ask fallows[
    if (random-float 100 < 50) and [weak-barrier] of patch-ahead 0.1 = true
    [set heading heading - 180]]

  ask fallows[
    if field = true and [non-field] of patch-ahead 3 = true
      [fd 0.7]]

  ask fallows with [fallow-region = 1][
    if [fallow-region] of patch-ahead 1 != 1 [set heading heading - 180 ]]

  find-herdmates
  if any? herdmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor = 0
        [ separate ]
        [ align
        cohere ]]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 7: FALLOW MOVEMENT PATTERN SUPPLEMENT CODE

to find-herdmates  ;; turtle procedure
  set herdmates other fallows in-radius 10 ; This should be other fallows
end

to find-nearest-neighbor ;; turtle procedure
  set nearest-neighbor min-one-of herdmates [distance myself]
end

;;; SEPARATE
to separate  ;; turtle procedure
  turn-away ([heading] of nearest-neighbor) 0
end

; align
to align  ;; turtle procedure
  turn-towards average-herdmate-heading 30.00
end

to-report average-herdmate-heading  ;; turtle procedure
  ;; We can't just average the heading variables here.
  ;; For example, the average of 1 and 359 should be 0,
  ;; not 180.  So we have to use trigonometry.
  let x-component sum [dx] of herdmates
  let y-component sum [dy] of herdmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

; cohere
to cohere  ;turtle procedure
  turn-towards average-heading-towards-herdmates 5
end

to-report average-heading-towards-herdmates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of herdmates
  let y-component mean [cos (towards myself + 180)] of herdmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

; helper procedures for fallow
to turn-towards [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings new-heading heading) max-turn
end

to turn-away [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings heading new-heading) max-turn
end

; turn right by "turn" degrees (or left if "turn" is negative),
; but never turn more than "max-turn" degrees
to turn-at-most [turn max-turn]  ;; turtle procedure
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 8: NEMATODE EGGS DEPOSITED BY DEER

;Eggs are produced at a given rate over time based on (Megyesi et al., 2020) and subseqent polynomial calculations by Brown TL
;ticks mod 31 refers to how often the deer defecates
;multiply by 46 because 105 eggs per gram faeces * 46 which is mean total weight of pellet faecal group for fallow deer

to eggspergram_fallow ;changed 31 to 27.2
  ask fallows with [ticks mod 27.2 = 0] [if ticks >= 0 and ticks <= 4392 ; period1
    [set egg-number (egg-number) + 105.45 * 46 ]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 4392 and ticks <= 8784  ; period2
    [set egg-number (egg-number) + 119.72 * 46]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 8784 and ticks <= 13176 ; period3
    [set egg-number (egg-number) + 135.89 * 46]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 13176 and ticks <= 17568 ; period4
    [set egg-number (egg-number) + 150.83 * 46 ]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 17568 and ticks <= 21960 ; period5
    [set egg-number (egg-number) + 161.39 * 46]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 21960 and ticks <= 26352 ; period6
    [set egg-number (egg-number) + 164.46 * 46 ]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 26352 and ticks <= 30744 ; period7
    [set egg-number (egg-number) + 156.89 * 46 ]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 30744 and ticks <= 35136 ; period8
    [set egg-number (egg-number) + 135.56 * 46]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 35136 and ticks <= 39528 ; period9
    [set egg-number (egg-number) + 97.32 * 46 ]]

   ask fallows with [ticks mod 27.2 = 0]  [if ticks >= 39528 and ticks <= 43920  ; period10
    [set egg-number (egg-number) + 39.06 * 46 ]]

end

to eggspergram_roe
  ask roes with [ticks mod 36 = 0] [if ticks >= 0 and ticks <= 4392 ; period1
    [set egg-number (egg-number) + 10.63 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 4392 and ticks <= 8784  ; period2
    [set egg-number (egg-number) + 38.02 * 20]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 8784 and ticks <= 13176 ; period3
    [set egg-number (egg-number) + 69.83 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 13176 and ticks <= 17568 ; period4
    [set egg-number (egg-number) + 101.58 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [ if ticks >= 17568 and ticks <= 21960 ; period5
    [set egg-number (egg-number) + 128.78 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 21960 and ticks <= 26352 ; period6
    [set egg-number (egg-number) + 146.91 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 26352 and ticks <= 30744 ; period7
    [set egg-number (egg-number) + 151.51 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [ if ticks >= 30744 and ticks <= 35136 ; period8
    [set egg-number (egg-number) + 138.05 * 20]]

   ask roes with [ticks mod 36 = 0]  [if ticks >= 35136 and ticks <= 39528 ; period9
    [set egg-number (egg-number) + 102.07 * 20 ]]

   ask roes with [ticks mod 36 = 0]  [ if ticks >= 39528 and ticks <= 43920  ; period10
    [set egg-number (egg-number) + 39.05 * 20 ]]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 9: VISUALISE MODEL

to visualise-eggs
  ask patches[
  if egg-number > 2000 [
      set pcolor 9.9 ]]

  ask patches[
  if egg-number > 4000 [
      set pcolor 109]]

  ask patches [
  if egg-number > 8000 [
      set pcolor 105 ]]

  ask patches [
  if egg-number > 16000 [
      set pcolor 100 ]]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 10: CONTINOUS LANDSCAPES

to setup-continuous_strong
 ask patches with [pxcor >= 11 and pxcor <= 101 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 30 and pxcor <= 37 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 39 and pxcor <= 46 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 48 and pxcor <= 55 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 57 and pxcor <= 64 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 66 and pxcor <= 73 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 75 and pxcor <= 82 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 84 and pxcor <= 91 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 93 and pxcor <= 100 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

end

to setup-continuous_weak
 ask patches with [pxcor >= 11 and pxcor <= 101 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 30 and pxcor <= 37 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 39 and pxcor <= 46 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 48 and pxcor <= 55 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 57 and pxcor <= 64 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 66 and pxcor <= 73 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 75 and pxcor <= 82 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 84 and pxcor <= 91 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 93 and pxcor <= 100 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

end

to setup-continuous_two_weak
 ask patches with [pxcor >= 11 and pxcor <= 101 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor black]

   ask patches with [pxcor >= 30 and pxcor <= 46 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor blue]

    ask patches with [pxcor >= 66 and pxcor <= 82 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 30 and pxcor <= 37 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 39 and pxcor <= 46 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 48 and pxcor <= 55 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 57 and pxcor <= 64 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 66 and pxcor <= 73 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 75 and pxcor <= 82 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 84 and pxcor <= 91 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 93 and pxcor <= 100 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

end

to setup-continuous_meadows
 ask patches with [pxcor >= 11 and pxcor <= 101 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor black]

   ask patches with [pxcor >= 30 and pxcor <= 46 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor yellow]

    ask patches with [pxcor >= 66 and pxcor <= 82 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor yellow]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 30 and pxcor <= 37 and pycor >= 44 and pycor <= 68]
 [set pcolor yellow]

   ask patches with [pxcor >= 39 and pxcor <= 46 and pycor >= 44 and pycor <= 68]
 [set pcolor yellow]

   ask patches with [pxcor >= 48 and pxcor <= 55 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 57 and pxcor <= 64 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 66 and pxcor <= 73 and pycor >= 44 and pycor <= 68]
 [set pcolor yellow]

   ask patches with [pxcor >= 75 and pxcor <= 82 and pycor >= 44 and pycor <= 68]
 [set pcolor yellow]

   ask patches with [pxcor >= 84 and pxcor <= 91 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

   ask patches with [pxcor >= 93 and pxcor <= 100 and pycor >= 44 and pycor <= 68]
 [set pcolor green]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 11: FRAGMENTED LANDSCAPES

to setup-fragmented_strong

  ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 17 and pycor <= 43] ;fence  bottom left
 [set pcolor black]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

   ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 69 and pycor <= 95] ;fence top left
 [set pcolor black]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 70 and pycor <= 94]
  [set pcolor green]

  ask patches with [pxcor >= 46 and pxcor <= 64 and pycor >= 43 and pycor <= 69] ;fence middle
 [set pcolor black]

  ask patches with [pxcor >= 47 and pxcor <= 54 and pycor >= 44 and pycor <= 68]
   [set pcolor green]
  ask patches with [pxcor >= 56 and pxcor <= 63 and pycor >= 44 and pycor <= 68]
  [set pcolor green]

   ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 69 and pycor <= 95] ;fence top right
 [set pcolor black]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 70 and pycor <= 94]
  [set pcolor green]


  ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 17 and pycor <= 43] ;fence bottom right
 [set pcolor black]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 18 and pycor <= 42]
   [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 18 and pycor <= 42]
  [set pcolor green]

end

to setup-fragmented_weak
   ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 70 and pycor <= 94]
  [set pcolor green]

   ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 70 and pycor <= 94]
  [set pcolor green]

  ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 18 and pycor <= 42]
   [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 18 and pycor <= 42]
  [set pcolor green]

  ask patches with [pxcor >= 46 and pxcor <= 64 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 47 and pxcor <= 54 and pycor >= 44 and pycor <= 68]
   [set pcolor green]

  ask patches with [pxcor >= 56 and pxcor <= 63 and pycor >= 44 and pycor <= 68]
  [set pcolor green]
end

to setup-fragmented_two_weak
   ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 70 and pycor <= 94]
  [set pcolor green]

   ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor blue]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 70 and pycor <= 94]
 [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 70 and pycor <= 94]
  [set pcolor green]

  ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 18 and pycor <= 42]
   [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 18 and pycor <= 42]
  [set pcolor green]

  ask patches with [pxcor >= 46 and pxcor <= 64 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 47 and pxcor <= 54 and pycor >= 44 and pycor <= 68]
   [set pcolor green]

  ask patches with [pxcor >= 56 and pxcor <= 63 and pycor >= 44 and pycor <= 68]
  [set pcolor green]
end

to setup-fragmented_meadows
   ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor yellow]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 70 and pycor <= 94]
 [set pcolor yellow]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 70 and pycor <= 94]
  [set pcolor yellow]

   ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 69 and pycor <= 95] ;fence
 [set pcolor yellow]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 70 and pycor <= 94]
 [set pcolor yellow]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 70 and pycor <= 94]
  [set pcolor yellow]

  ask patches with [pxcor >= 11 and pxcor <= 29 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 12 and pxcor <= 19 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 21 and pxcor <= 28 and pycor >= 18 and pycor <= 42]
    [set pcolor green]

  ask patches with [pxcor >= 81 and pxcor <= 99 and pycor >= 17 and pycor <= 43] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 82 and pxcor <= 89 and pycor >= 18 and pycor <= 42]
   [set pcolor green]

  ask patches with [pxcor >= 91 and pxcor <= 98 and pycor >= 18 and pycor <= 42]
  [set pcolor green]

  ask patches with [pxcor >= 46 and pxcor <= 64 and pycor >= 43 and pycor <= 69] ;fence
 [set pcolor black]

  ask patches with [pxcor >= 47 and pxcor <= 54 and pycor >= 44 and pycor <= 68]
   [set pcolor green]

  ask patches with [pxcor >= 56 and pxcor <= 63 and pycor >= 44 and pycor <= 68]
  [set pcolor green]
end
@#$#@#$#@
GRAPHICS-WINDOW
190
10
593
414
-1
-1
3.5
1
15
1
1
1
0
0
0
1
0
112
0
112
1
1
1
ticks
30.0

BUTTON
5
10
68
55
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
70
10
133
55
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
60
137
105
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
600
10
833
55
landscape
landscape
"fragmented_strong" "fragmented_weak" "fragmented_two_weak" "fragmented_meadows" "continuous_strong" "continuous_weak" "continuous_two_weak" "continuous_meadows"
3

SLIDER
600
55
699
88
wild_pop
wild_pop
0
5
5.0
1
1
NIL
HORIZONTAL

CHOOSER
600
91
692
136
ruminant-type
ruminant-type
"Roe" "Fallow"
1

TEXTBOX
13
252
173
403
1 patch = 10 square meters\n43920 ticks = 61 days\n720 ticks = 1 day\n1 tick = 2 minutes\n\nroe - 1360 meters per day\nfallow - 1630 meters per day\n
11
0.0
1

SLIDER
600
138
756
171
count-down
count-down
0
43920
49715.0
5
1
NIL
HORIZONTAL

MONITOR
8
155
123
200
time
count-down - ticks
0
1
11

BUTTON
5
110
62
143
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## MODEL NAME

NemaSim 1.0

## WHAT IS IT?

Simulates the transmission of gastro-intestinal nematode eggs from deer to livestock pasture. Further R code plugin then estimates number of eggs which develop to adult nematodes in sheep flocks at given flocking densities. 




@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

moose
false
0
Polygon -7500403 true true 196 228 198 297 180 297 178 244 166 213 136 213 106 213 79 227 73 259 50 257 49 229 38 197 26 168 26 137 46 120 101 122 147 102 181 111 217 121 256 136 294 151 286 169 256 169 241 198 211 188
Polygon -7500403 true true 74 258 87 299 63 297 49 256
Polygon -7500403 true true 25 135 15 186 10 200 23 217 25 188 35 141
Polygon -7500403 true true 270 150 253 100 231 94 213 100 208 135
Polygon -7500403 true true 225 120 204 66 207 29 185 56 178 27 171 59 150 45 165 90
Polygon -7500403 true true 225 120 249 61 241 31 265 56 272 27 280 59 300 45 285 90

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Roe continous" repetitions="40" runMetricsEveryStep="false">
    <setup>reset
setup</setup>
    <go>go</go>
    <final>;counter is a global variable and is automatically set to zero, and increases by one for each file name
let filename (word "croe" counter ".csv")
  csv:to-file filename [ (list pxcor pycor egg-number) ] of patches
  set counter counter  + 1
  if file-exists? filename[
    set filename (word "file_" counter ".csv")]</final>
    <enumeratedValueSet variable="wild_pop">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count-down">
      <value value="49715"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ruminant-type">
      <value value="&quot;Roe&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape">
      <value value="&quot;continuous_strong&quot;"/>
      <value value="&quot;continuous_weak&quot;"/>
      <value value="&quot;continuous_two_weak&quot;"/>
      <value value="&quot;continuous_meadows&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Roe fragment" repetitions="40" runMetricsEveryStep="false">
    <setup>reset
setup</setup>
    <go>go</go>
    <final>;counter is a global variable and is automatically set to zero, and increases by one for each file name
let filename (word "froe" counter ".csv")
  csv:to-file filename [ (list pxcor pycor egg-number) ] of patches
  set counter counter  + 1
  if file-exists? filename[
    set filename (word "stop_" counter ".csv")]</final>
    <enumeratedValueSet variable="wild_pop">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count-down">
      <value value="49715"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ruminant-type">
      <value value="&quot;Roe&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape">
      <value value="&quot;fragmented_strong&quot;"/>
      <value value="&quot;fragmented_weak&quot;"/>
      <value value="&quot;fragmented_two_weak&quot;"/>
      <value value="&quot;fragmented_meadows&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Fallow continous" repetitions="40" runMetricsEveryStep="false">
    <setup>reset
setup</setup>
    <go>go</go>
    <final>;counter is a global variable and is automatically set to zero, and increases by one for each file name
let filename (word "cfallow" counter ".csv")
  csv:to-file filename [ (list pxcor pycor egg-number) ] of patches
  set counter counter  + 1
  if file-exists? filename[
    set filename (word "file_" counter ".csv")]</final>
    <enumeratedValueSet variable="wild_pop">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count-down">
      <value value="49715"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ruminant-type">
      <value value="&quot;Fallow&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape">
      <value value="&quot;continuous_strong&quot;"/>
      <value value="&quot;continuous_weak&quot;"/>
      <value value="&quot;continuous_two_weak&quot;"/>
      <value value="&quot;continuous_meadows&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Fallow fragment" repetitions="40" runMetricsEveryStep="false">
    <setup>reset
setup</setup>
    <go>go</go>
    <final>;counter is a global variable and is automatically set to zero, and increases by one for each file name
let filename (word "ffallow" counter ".csv")
  csv:to-file filename [ (list pxcor pycor egg-number) ] of patches
  set counter counter  + 1
  if file-exists? filename[
    set filename (word "stop_" counter ".csv")]</final>
    <enumeratedValueSet variable="wild_pop">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count-down">
      <value value="49715"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ruminant-type">
      <value value="&quot;Fallow&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape">
      <value value="&quot;fragmented_strong&quot;"/>
      <value value="&quot;fragmented_weak&quot;"/>
      <value value="&quot;fragmented_two_weak&quot;"/>
      <value value="&quot;fragmented_meadows&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
