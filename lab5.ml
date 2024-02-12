(*
                              CS51 Lab 5
           Variants, algebraic types, and pattern matching
 *)

(*
                               SOLUTION
 *)

(*======================================================================
Part 1: Colors as an algebraic data type

In this lab you'll use algebraic data types to create several data
structures.

Ultimately, you'll define several types and structures that allow you to
create a family tree. To do this, you need to create a type to store a
set of biographical information about a person, like name, birthdate,
and favorite color. This set of data is different from the enrollment
data from the prior lab, so you'll need to create a new type.

You might be tempted to do something simple like

  type person = { name : string; favorite : string; birthday : string } ;;

Let's consider why this may not be appropriate by evaluating the type
for each record field individually.

It seems reasonable for a name to be a string (though personal name
structures can be quite complex; see
<https://en.wikipedia.org/wiki/Personal_name>), so let's declare that
complete and move on.

The "favorite" field is more problematic. Although we named it such for
simplicity, it doesn't convey very well that we intended for this field
to represent a person's favorite *color*. This could be resolved with
some documentation, but is not enforced at any level other than hope.
Further, it's very likely that many people would select one of a subset
of simple colors. Let's fix this issue first.

........................................................................
Exercise 1: Define a new type, called `color_label`, whose values can be
any of the following: red, orange, yellow, green, blue, indigo, or
violet.
......................................................................*)

type color_label =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet ;;

(* This is a good start, but doesn't allow for definition of *all* of
the colors that, say, a computer display might be able to present. Let's
make it more expressive.

One of the most commonly used methods of representing color in digital
devices is as an "RGB" value: a triplet of values to represent red,
green, and blue components that, through additive mixing, produce the
wide array of colors our devices render.

Commonly, each of the red, green, and blue values are made up of a
single 8-bit (1-byte) integer. Since one byte represents 256 discrete
values, there are over 16.7 million (256 * 256 * 256) possible colors
that can be represented with this method.

The three components that make up an RGB color are referred to as
"channels". In this 8-bit-per-channel model, a value of 0 represents no
color and a value of 255 represents the full intensity of that color.
Some examples:

     R  |  G  |  B  | Color
    ----|-----|-----|------------
    255 |   0 |   0 | Red
    164 |  16 |  52 | Crimson 
    255 | 165 |   0 | Orange
    255 | 255 |   0 | Yellow
      0 |  64 |   0 | Dark green
      0 | 255 |   0 | Green
      0 | 255 | 255 | Cyan
      0 |   0 | 255 | Blue
     75 |   0 | 130 | Indigo
    240 | 130 | 240 | Violet

........................................................................
Exercise 2: Define a color type that supports either `Simple` colors
(from the `color_label` type you defined previously) or `RGB` colors,
which would incorporate a tuple of values for the three color channels.
You'll want to use `Simple` and `RGB` as the value constructors in this
new variant type.
......................................................................*)

type color =
  | Simple of color_label
  | RGB of int * int * int ;;

(* There is an important assumption about the RGB values that determine
whether a color is valid or not. The RGB type presupposes an
*invariant*, that is, a condition that we assume to be true in order for
the type to be valid:

  The red, green, and blue channels must be a non-negative 8-bit int.
  Therefore, each channel must be in the range [0, 255].

Since OCaml, unlike some other languages, does not have native support
for unsigned 8-bit integers, you should ensure the invariant remains
true in your code. (You might think to use the OCaml `char` type --
which is an 8-bit character -- but this would be an abuse of the type.
In any case, thinking about invariants will be useful practice for
upcoming problem sets.)

We'll want a function to validate the invariant for RGB color values.
(Simple colors are assumed always valid.) There are several approaches
to building such functions, which differ in their types, and for which
we'll use different naming conventions:

* valid_rgb : color -> bool -- Returns true if the color argument is
  valid, false otherwise.

* validated_rgb : color -> color -- Returns its argument unchanged if it
  is a valid color, and raises an appropriate exception otherwise.

* validate_rgb : color -> unit -- Returns unit; raises an appropriate
  exception if its argument is not a valid color.

The name prefixes "valid_", "validated_", and "validate_" are intended
to be indicative of the different approaches to validation.

In this lab, we'll use the "validated_" approach and naming convention,
though you may want to think about the alternatives. In the next lab, we
use the "valid_" alternative approach.

........................................................................
Exercise 3: Write a function `validated_rgb` that accepts a `color` and
returns that color unchanged if it's valid. However, if its argument is
not a valid color (that is, the invariant is violated), it raises an
`Invalid_color` exception (defined below) with a useful message.
......................................................................*)

exception Invalid_color of string ;;

let validated_rgb (color : color) : color =
  let bad (channel : int) : bool =
    channel < 0 || channel > 255 in
  match color with
  | Simple _label -> color
  | RGB (r, g, b) ->
     if bad r then raise (Invalid_color "bad red channel")
     else if bad g then raise (Invalid_color "bad green channel")
     else if bad b then raise (Invalid_color "bad blue channel")
     else color ;;

(*......................................................................
Exercise 4: Write a function `make_color` that accepts three integers
for the channel values and returns a value of the color type. Be sure
to verify the invariant.
......................................................................*)

let make_color (r : int) (g : int) (b : int) : color =
  validated_rgb (RGB (r, g, b)) ;;

(*......................................................................
Exercise 5: Write a function `rgb_of_color` that accepts a `color` and
returns a 3-tuple of integers representing that color. This is trivial
for `RGB` colors, but not quite so easy for the hard-coded `Simple`
colors. Fortunately, we've already provided RGB values for simple
colors in the table above.
......................................................................*)

let rgb_of_color (c : color) : int * int * int =
  match c with
  | RGB (r, g, b) -> (r, g, b)
  | Simple x ->
     match x with
     | Red     -> (255,   0,   0)
     | Orange  -> (255, 165,   0)
     | Yellow  -> (255, 255,   0)
     | Green   -> (  0, 255,   0)
     | Blue    -> (  0,   0, 255)
     | Indigo  -> ( 75,   0, 130)
     | Violet  -> (240, 130, 240) ;;

(*======================================================================
Part 2: Dates as a record type

Now let's move on to the last data type that will be used in the
biographical data type: the date field.

Above, we naively proposed a string for the date field. Does this make
sense for this field? Arguably not, since it will make comparison and
calculation extremely difficult.

Dates are frequently needed in programming, and OCaml (like many
languages) supports them through a library module; the `Unix` module
provides a `tm` data type for dates and times. Normally, we would 
reduce duplication of code by relying on that module (the edict of
irredundancy), but for the sake of practice you'll develop your 
own simple version.

........................................................................
Exercise 6: Create a type `date` that supports values for years,
months, and days. First, consider what types of data each value should
be. Then, consider the implications of representing the overall data
type as a tuple or a record.
......................................................................*)

(* The `Unix` module provides for dates and times through a record
   type, `tm`. There are other plausible alternatives, such as a
   triple of type `int * int * int`, though the record type has a nice
   self-documenting aspect in the labels. *)

type date = { year : int; month : int; day : int } ;;

(* After you've thought it through, go to <http://url.cs51.io/lab5-1> to
see how we implemented the `date` type. If you picked differently, why 
did you choose that way? Why might our approach be preferable?

........................................................................
Exercise 7: Change your `date` data type, above, to implement it in a
manner identical to our method. If no changes are required...well, that
was easy.
........................................................................

Like the color type, above, date values obey invariants. In fact, the
invariants for this type are more complex: we must ensure that days
fall within an allowable range depending on the month, and even on the
year.

The invariants are as follows:

- For our purposes, we'll only support non-negative years.

- January, March, May, July, August, October, and December have 31
  days.

- April, June, September, and November have 30 days.

- February has 28 days in common years, 29 days in leap years.

- Leap years are years that can be divided by 4, but not by 100,
  unless by 400.

You may find Wikipedia's leap year algorithm pseudocode useful:
<https://en.wikipedia.org/wiki/Leap_year#Algorithm>

........................................................................
Exercise 8: Create a `validated_date` function that raises
`Invalid_date` if the invariant is violated, and returns the date
unchanged if valid.
......................................................................*)

exception Invalid_date of string ;;

let validated_date ({year; month; day} as date) : date =
  if year < 0 then raise (Invalid_date "negative year")
  else
    let is_leap = (year mod 4 = 0 && year mod 100 <> 0)
                  || year mod 400 = 0 in
    let max_days = 
      match month with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 ->  31
      | 4 | 6 | 9 | 11              ->  30
      | 2           -> if is_leap then  29
                       else             28
      | _ -> raise (Invalid_date "bad month") in
    if day > max_days then raise (Invalid_date "day too large")
    else if day < 1 then raise (Invalid_date "day too small")
    else date ;;

(* Note the use of "field punning" to simplify the pattern match for
   the record. By using just the labels in the record, we implicitly
   define variables for the field values of the same name as the
   labels: `year`, `month`, and `day`.

   This code also makes use of a useful previously unseen construct of
   OCaml's matching, which allows the naming of the parts of a data
   item and the whole item as well. In the pattern `{year; month; day}
   as date`, the result of the pattern is to name the whole record
   `date` while still allowing for accessing the parts with names
   `year`, `month`, and `day`.

   An issue to consider is the priority of the various invariants. It
   makes sense to check the validity of the year and the month before
   moving on to check the validity of the day, as we've done here,
   since the validity of the day relies on the validity of the month
   and year. That is, in case the year or month is invalid, we'd
   rather raise an exception complaining about that than one
   complaining about an invalid day. *)
   
(*......................................................................
Exercise 9: Define a function `string_of_date` that returns a string
representing its date argument. For example,

    # string_of_date {year = 1706; month = 1; day = 17} ;;
    - : string = "January 17, 1706"
......................................................................*)

(* The trickiest part is converting the month number into a month name
   string. One method is to index directly into a list of month names,
   for example, the name for month 5 can be found at index 4 in this
   0-indexed list:

      # List.nth ["January"; "February"; "March"; "April"; 
                  "May"; "June"; "July"; "August";
                  "September"; "October"; "November"; "December"]
                 4 ;;
      - : string = "May"

   We use this technique below. 

   Putting the components together can make use of the string
   concatenation operator `^`, leading to this implementation:

      let string_of_date ({year; month; day} : date) : string =
        let month_names = ["January"; "February"; "March"; "April"; 
                           "May"; "June"; "July"; "August";
                           "September"; "October"; "November"; "December"] in
        (List.nth month_names (pred month))
        ^ " "
        ^ string_of_int day
        ^ ", "
        ^ string_of_int year ;;

   Alternatively, functions in the formatted printing module `Printf`
   are useful to know about for this kind of thing: *)
  
let string_of_date ({year; month; day} : date) : string =
  let month_names = ["January"; "February"; "March"; "April"; 
                     "May"; "June"; "July"; "August";
                     "September"; "October"; "November"; "December"] in
  Printf.sprintf "%s %d, %d" (List.nth month_names (pred month)) day year ;;
     
(*======================================================================
Part 3: Persons as an algebraic data type

Now, combine all of these different types to define a person record,
with a name, a favorite color, and a birthdate.

........................................................................
Exercise 10: Define a `person` record type. Use the field names
`name`, `favorite`, and `birthdate`.
......................................................................*)
                 
type person = {name : string; favorite : color; birthdate : date} ;;

(* Just for fun, here's a function to open up a graphics window and
display a person's information. Try it out! *)

(* display_person person -- Displays `person`'s name and birth date in
   a graphics window on a background of their favorite color. *)

let display_person ({name; favorite; birthdate} : person) : unit =
  let open Graphics in
  open_graph "";
  resize_window 200 60;
  let r, g, b = rgb_of_color favorite in
  set_color (rgb r g b);
  fill_rect 0 0 200 60;
  set_color white;
  moveto 20 40;
  draw_string name;
  moveto 20 20;
  draw_string (string_of_date birthdate);
  ignore (read_key ()) ;;
  
(*
let () = display_person {name = "Ben Franklin";
                         favorite = Simple Blue;
                         birthdate = {year = 1706; month = 1; day = 17}} ;;
 *)
