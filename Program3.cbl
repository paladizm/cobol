       identification division.
       program-id. Program3.
       author. Zac_Paladino.

       environment division.
       input-output section.
       file-control. select guest-data assign to "guests.dat"
                           organization is line sequential.
                     select guest-listing assign to "guestOut.txt"
                           organization is line sequential.
       data division.
       file section.
       fd guest-data.
       01 guest-record.
           05 guest-in         pic X(20).
           05 nights-in        pic 99.
           05 room-dis-in      pic X.
           05 local-calls-in   pic 99.
           05 long-calls-in    pic 9(03)V99.

       fd guest-listing.
       01 listing-out          pic X(80).
       working-storage section.
       01  are-there-more-records  pic XXX value "yes".
       01  variable-listing.
           05 night-factor     pic 99 value zeros.
           05 night-calc       pic 99 value zeros.
           05 night-var        pic 99 value zeros.
           05 room-factor      pic 9(8)V99 value zeros.
           05 total-factor     pic 9(8)V99 value zeros.
           05 call-count       pic 99 value zeros.
           05 ln-count         pic 99 value zeros.
           05 page-count       pic 99 value 1.
       01 variables-totals.
           05 total-telephone  pic 9(4)V99 value zeros.
           05 total-room-char  pic 9(8)V99 value zeros.
           05 total-charge     pic 9(8)V99  value zeros.
           05 total-local-char pic 9(4)V99 value zeros.
       01  print-holiday-header.
           05                  pic X(30) value spaces.
           05 HL-Holiday       pic X(31) value spaces.
       01  print-header.
           05                  pic X(39) value spaces.
           05 HL-Billing       pic X(14) value "BILLING REPORT".
       01  print-column-header1.
           05                  pic X(5) value spaces.
           05 HL-guest         pic X(5) value "GUEST".
           05                  pic X(17) value spaces.
           05 HL-nights        pic X(6) value "NIGHTS".
           05                  pic X(3) value spaces.
           05 HL-room          pic X(5) value "ROOM".
           05                  pic X(3) value spaces.
           05 HL-total1        pic X(5) value "TOTAL".
           05                  pic X(6)  value spaces.
           05 HL-total2        pic X(5) value "TOTAL".
           05                  pic X(6) value spaces.
           05 HL-total3        pic X(5) value "TOTAL".
       01  print-column-header2.
           05                  pic X(5) value spaces.
           05 HL-name          pic X(5) value "NAME".
           05                  pic X(26) value spaces.
           05 HL-type          pic X(5) value "TYPE".
           05                  pic X(3) value spaces.
           05 HL-room2         pic X(5) value "ROOM".
           05                  pic X(6) value spaces.
           05 HL-telephone     pic X(9) value "TELEPHONE".
           05                  pic X(2) value spaces.
           05 HL-charge        pic X(6) value "CHARGE".
       01  print-column-header3.
           05                  pic X(44) value spaces.
           05 HL-charge2       pic X(6) value "CHARGE".
           05                  pic X(5) value spaces.
           05 HL-charge3       pic X(6) value "CHARGE".
           05                  pic X(5) value spaces.
           05 HL-tax           pic X(14) value "(8.5% TAX INC)".
       01  print-page.
           05  HL-page         pic ZZ.
       01  print-listing.
           05                  pic X(5).
           05 guest-out        pic X(20).
           05                  pic X(5).
           05 nights-out       pic ZZ.
           05                  pic X(4).
           05 room-type-out    pic X.
           05                  pic X(4).
           05 total-room-out   pic $(8).99.
           05                  pic X(5).
           05 telephone-out    pic $(5).99.
           05                  pic X(1).
           05 total-charge-out pic $(8).99.
       procedure division.
       100-main-module.
           open input guest-data
                   output guest-listing
           perform 300-header-module
           perform until are-there-more-records = "no "
               read guest-data
                   at end
                       move "no " to are-there-more-records
                   not at end
                       perform 200-page-module
                       perform 400-procedure-module
               end-read
           end-perform
           close guest-data
                   guest-listing
           stop run.
       200-page-module.
           If ln-count >= 25
                  move page-count to HL-page
                   write listing-out from print-page
                      After advancing page
                         add 1 to page-count
                         move zeros to ln-count
                         perform 300-header-module
            end-if.
       300-header-module.
           move spaces to listing-out
           move "***HAPPY HOLIDAYS RESORT INN***" to HL-holiday
           write listing-out from print-holiday-header
           write listing-out from print-header
               before advancing 2 line
           write listing-out from print-column-header1
           write listing-out from print-column-header2
           write listing-out from print-column-header3.
       400-procedure-module.
           move spaces to listing-out
           move spaces to print-listing
           move guest-in to guest-out
           move nights-in to nights-out
           move room-dis-in to room-type-out
           perform 500-totalling-module
           write listing-out from print-listing
               After advancing 1 line
           add 1 to ln-count.
       500-totalling-module.
           move nights-out to night-calc
           move nights-out to night-var
           compute night-factor = night-var / 5
           compute night-calc = night-calc - night-factor
           if room-type-out = "S"
              Then
               compute total-room-char rounded = night-calc * 52.75
               compute room-factor rounded = 26.38 * night-factor
               compute total-room-char = total-room-char + room-factor
               move total-room-char to total-room-out
              end-if
            if room-type-out = "D"
              Then
               compute total-room-char rounded = night-calc * 72.25
               compute room-factor rounded = 36.13 * night-factor
               compute total-room-char = total-room-char + room-factor
               move total-room-char to total-room-out
              end-if
            if room-type-out = "L"
              Then
               compute total-room-char rounded = night-calc * 119.00
               compute room-factor rounded = 59.50 * night-factor
               compute total-room-char = total-room-char + room-factor
               move total-room-char to total-room-out
              end-if
            move zeros to room-factor
            move zeros to night-factor
            move local-calls-in to call-count
            compute call-count = call-count - 4
            if call-count > 0
               Then
                compute total-local-char rounded = 0.50 * call-count
            end-if
            move long-calls-in to total-telephone
            compute total-telephone = total-telephone + total-local-char
            move total-telephone to telephone-out
            compute total-charge = total-room-char + total-telephone
            compute total-factor rounded = total-charge * .085
            compute total-charge = total-charge + total-factor
            move total-charge to total-charge-out.










