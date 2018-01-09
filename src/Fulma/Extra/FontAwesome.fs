namespace Fulma.Extra

open Fable.Core
open Fable.Helpers.React
open Fulma.Elements
open Fulma

module FontAwesome =

    [<RequireQualifiedAccess>]
    module Fa =
        module I =
            type [<Fable.Core.CompileAsArray>] IFontAwesomeIcon =
                interface end

            // In order to update this DU please us the script located under: utils/extract-font-awesome.js
            // Generation for Font awesome 4.7.0
            [<StringEnum>]
            type [<Fable.Core.CompileAsArray>] FontAwesomeIcons =
                // | [<CompiledName("fa-500px")>] 500px
                | [<CompiledName("fa-address-book")>] AddressBook
                | [<CompiledName("fa-address-book-o")>] AddressBookO
                | [<CompiledName("fa-address-card")>] AddressCard
                | [<CompiledName("fa-address-card-o")>] AddressCardO
                | [<CompiledName("fa-adjust")>] Adjust
                | [<CompiledName("fa-adn")>] Adn
                | [<CompiledName("fa-align-center")>] AlignCenter
                | [<CompiledName("fa-align-justify")>] AlignJustify
                | [<CompiledName("fa-align-left")>] AlignLeft
                | [<CompiledName("fa-align-right")>] AlignRight
                | [<CompiledName("fa-amazon")>] Amazon
                | [<CompiledName("fa-ambulance")>] Ambulance
                | [<CompiledName("fa-american-sign-language-interpreting")>] AmericanSignLanguageInterpreting
                | [<CompiledName("fa-anchor")>] Anchor
                | [<CompiledName("fa-android")>] Android
                | [<CompiledName("fa-angellist")>] Angellist
                | [<CompiledName("fa-angle-double-down")>] AngleDoubleDown
                | [<CompiledName("fa-angle-double-left")>] AngleDoubleLeft
                | [<CompiledName("fa-angle-double-right")>] AngleDoubleRight
                | [<CompiledName("fa-angle-double-up")>] AngleDoubleUp
                | [<CompiledName("fa-angle-down")>] AngleDown
                | [<CompiledName("fa-angle-left")>] AngleLeft
                | [<CompiledName("fa-angle-right")>] AngleRight
                | [<CompiledName("fa-angle-up")>] AngleUp
                | [<CompiledName("fa-apple")>] Apple
                | [<CompiledName("fa-archive")>] Archive
                | [<CompiledName("fa-area-chart")>] AreaChart
                | [<CompiledName("fa-arrow-circle-down")>] ArrowCircleDown
                | [<CompiledName("fa-arrow-circle-left")>] ArrowCircleLeft
                | [<CompiledName("fa-arrow-circle-o-down")>] ArrowCircleODown
                | [<CompiledName("fa-arrow-circle-o-left")>] ArrowCircleOLeft
                | [<CompiledName("fa-arrow-circle-o-right")>] ArrowCircleORight
                | [<CompiledName("fa-arrow-circle-o-up")>] ArrowCircleOUp
                | [<CompiledName("fa-arrow-circle-right")>] ArrowCircleRight
                | [<CompiledName("fa-arrow-circle-up")>] ArrowCircleUp
                | [<CompiledName("fa-arrow-down")>] ArrowDown
                | [<CompiledName("fa-arrow-left")>] ArrowLeft
                | [<CompiledName("fa-arrow-right")>] ArrowRight
                | [<CompiledName("fa-arrow-up")>] ArrowUp
                | [<CompiledName("fa-arrows")>] Arrows
                | [<CompiledName("fa-arrows-alt")>] ArrowsAlt
                | [<CompiledName("fa-arrows-h")>] ArrowsH
                | [<CompiledName("fa-arrows-v")>] ArrowsV
                | [<CompiledName("fa-asl-interpreting")>] AslInterpreting
                | [<CompiledName("fa-assistive-listening-systems")>] AssistiveListeningSystems
                | [<CompiledName("fa-asterisk")>] Asterisk
                | [<CompiledName("fa-at")>] At
                | [<CompiledName("fa-audio-description")>] AudioDescription
                | [<CompiledName("fa-automobile")>] Automobile
                | [<CompiledName("fa-backward")>] Backward
                | [<CompiledName("fa-balance-scale")>] BalanceScale
                | [<CompiledName("fa-ban")>] Ban
                | [<CompiledName("fa-bandcamp")>] Bandcamp
                | [<CompiledName("fa-bank")>] Bank
                | [<CompiledName("fa-bar-chart")>] BarChart
                | [<CompiledName("fa-bar-chart-o")>] BarChartO
                | [<CompiledName("fa-barcode")>] Barcode
                | [<CompiledName("fa-bars")>] Bars
                | [<CompiledName("fa-bath")>] Bath
                | [<CompiledName("fa-bathtub")>] Bathtub
                | [<CompiledName("fa-battery")>] Battery
                | [<CompiledName("fa-battery-0")>] Battery0
                | [<CompiledName("fa-battery-1")>] Battery1
                | [<CompiledName("fa-battery-2")>] Battery2
                | [<CompiledName("fa-battery-3")>] Battery3
                | [<CompiledName("fa-battery-4")>] Battery4
                | [<CompiledName("fa-battery-empty")>] BatteryEmpty
                | [<CompiledName("fa-battery-full")>] BatteryFull
                | [<CompiledName("fa-battery-half")>] BatteryHalf
                | [<CompiledName("fa-battery-quarter")>] BatteryQuarter
                | [<CompiledName("fa-battery-three-quarters")>] BatteryThreeQuarters
                | [<CompiledName("fa-bed")>] Bed
                | [<CompiledName("fa-beer")>] Beer
                | [<CompiledName("fa-behance")>] Behance
                | [<CompiledName("fa-behance-square")>] BehanceSquare
                | [<CompiledName("fa-bell")>] Bell
                | [<CompiledName("fa-bell-o")>] BellO
                | [<CompiledName("fa-bell-slash")>] BellSlash
                | [<CompiledName("fa-bell-slash-o")>] BellSlashO
                | [<CompiledName("fa-bicycle")>] Bicycle
                | [<CompiledName("fa-binoculars")>] Binoculars
                | [<CompiledName("fa-birthday-cake")>] BirthdayCake
                | [<CompiledName("fa-bitbucket")>] Bitbucket
                | [<CompiledName("fa-bitbucket-square")>] BitbucketSquare
                | [<CompiledName("fa-bitcoin")>] Bitcoin
                | [<CompiledName("fa-black-tie")>] BlackTie
                | [<CompiledName("fa-blind")>] Blind
                | [<CompiledName("fa-bluetooth")>] Bluetooth
                | [<CompiledName("fa-bluetooth-b")>] BluetoothB
                | [<CompiledName("fa-bold")>] Bold
                | [<CompiledName("fa-bolt")>] Bolt
                | [<CompiledName("fa-bomb")>] Bomb
                | [<CompiledName("fa-book")>] Book
                | [<CompiledName("fa-bookmark")>] Bookmark
                | [<CompiledName("fa-bookmark-o")>] BookmarkO
                | [<CompiledName("fa-braille")>] Braille
                | [<CompiledName("fa-briefcase")>] Briefcase
                | [<CompiledName("fa-btc")>] Btc
                | [<CompiledName("fa-bug")>] Bug
                | [<CompiledName("fa-building")>] Building
                | [<CompiledName("fa-building-o")>] BuildingO
                | [<CompiledName("fa-bullhorn")>] Bullhorn
                | [<CompiledName("fa-bullseye")>] Bullseye
                | [<CompiledName("fa-bus")>] Bus
                | [<CompiledName("fa-buysellads")>] Buysellads
                | [<CompiledName("fa-cab")>] Cab
                | [<CompiledName("fa-calculator")>] Calculator
                | [<CompiledName("fa-calendar")>] Calendar
                | [<CompiledName("fa-calendar-check-o")>] CalendarCheckO
                | [<CompiledName("fa-calendar-minus-o")>] CalendarMinusO
                | [<CompiledName("fa-calendar-o")>] CalendarO
                | [<CompiledName("fa-calendar-plus-o")>] CalendarPlusO
                | [<CompiledName("fa-calendar-times-o")>] CalendarTimesO
                | [<CompiledName("fa-camera")>] Camera
                | [<CompiledName("fa-camera-retro")>] CameraRetro
                | [<CompiledName("fa-car")>] Car
                | [<CompiledName("fa-caret-down")>] CaretDown
                | [<CompiledName("fa-caret-left")>] CaretLeft
                | [<CompiledName("fa-caret-right")>] CaretRight
                | [<CompiledName("fa-caret-square-o-down")>] CaretSquareODown
                | [<CompiledName("fa-caret-square-o-left")>] CaretSquareOLeft
                | [<CompiledName("fa-caret-square-o-right")>] CaretSquareORight
                | [<CompiledName("fa-caret-square-o-up")>] CaretSquareOUp
                | [<CompiledName("fa-caret-up")>] CaretUp
                | [<CompiledName("fa-cart-arrow-down")>] CartArrowDown
                | [<CompiledName("fa-cart-plus")>] CartPlus
                | [<CompiledName("fa-cc")>] Cc
                | [<CompiledName("fa-cc-amex")>] CcAmex
                | [<CompiledName("fa-cc-diners-club")>] CcDinersClub
                | [<CompiledName("fa-cc-discover")>] CcDiscover
                | [<CompiledName("fa-cc-jcb")>] CcJcb
                | [<CompiledName("fa-cc-mastercard")>] CcMastercard
                | [<CompiledName("fa-cc-paypal")>] CcPaypal
                | [<CompiledName("fa-cc-stripe")>] CcStripe
                | [<CompiledName("fa-cc-visa")>] CcVisa
                | [<CompiledName("fa-certificate")>] Certificate
                | [<CompiledName("fa-chain")>] Chain
                | [<CompiledName("fa-chain-broken")>] ChainBroken
                | [<CompiledName("fa-check")>] Check
                | [<CompiledName("fa-check-circle")>] CheckCircle
                | [<CompiledName("fa-check-circle-o")>] CheckCircleO
                | [<CompiledName("fa-check-square")>] CheckSquare
                | [<CompiledName("fa-check-square-o")>] CheckSquareO
                | [<CompiledName("fa-chevron-circle-down")>] ChevronCircleDown
                | [<CompiledName("fa-chevron-circle-left")>] ChevronCircleLeft
                | [<CompiledName("fa-chevron-circle-right")>] ChevronCircleRight
                | [<CompiledName("fa-chevron-circle-up")>] ChevronCircleUp
                | [<CompiledName("fa-chevron-down")>] ChevronDown
                | [<CompiledName("fa-chevron-left")>] ChevronLeft
                | [<CompiledName("fa-chevron-right")>] ChevronRight
                | [<CompiledName("fa-chevron-up")>] ChevronUp
                | [<CompiledName("fa-child")>] Child
                | [<CompiledName("fa-chrome")>] Chrome
                | [<CompiledName("fa-circle")>] Circle
                | [<CompiledName("fa-circle-o")>] CircleO
                | [<CompiledName("fa-circle-o-notch")>] CircleONotch
                | [<CompiledName("fa-circle-thin")>] CircleThin
                | [<CompiledName("fa-clipboard")>] Clipboard
                | [<CompiledName("fa-clock-o")>] ClockO
                | [<CompiledName("fa-clone")>] Clone
                | [<CompiledName("fa-close")>] Close
                | [<CompiledName("fa-cloud")>] Cloud
                | [<CompiledName("fa-cloud-download")>] CloudDownload
                | [<CompiledName("fa-cloud-upload")>] CloudUpload
                | [<CompiledName("fa-cny")>] Cny
                | [<CompiledName("fa-code")>] Code
                | [<CompiledName("fa-code-fork")>] CodeFork
                | [<CompiledName("fa-codepen")>] Codepen
                | [<CompiledName("fa-codiepie")>] Codiepie
                | [<CompiledName("fa-coffee")>] Coffee
                | [<CompiledName("fa-cog")>] Cog
                | [<CompiledName("fa-cogs")>] Cogs
                | [<CompiledName("fa-columns")>] Columns
                | [<CompiledName("fa-comment")>] Comment
                | [<CompiledName("fa-comment-o")>] CommentO
                | [<CompiledName("fa-commenting")>] Commenting
                | [<CompiledName("fa-commenting-o")>] CommentingO
                | [<CompiledName("fa-comments")>] Comments
                | [<CompiledName("fa-comments-o")>] CommentsO
                | [<CompiledName("fa-compass")>] Compass
                | [<CompiledName("fa-compress")>] Compress
                | [<CompiledName("fa-connectdevelop")>] Connectdevelop
                | [<CompiledName("fa-contao")>] Contao
                | [<CompiledName("fa-copy")>] Copy
                | [<CompiledName("fa-copyright")>] Copyright
                | [<CompiledName("fa-creative-commons")>] CreativeCommons
                | [<CompiledName("fa-credit-card")>] CreditCard
                | [<CompiledName("fa-credit-card-alt")>] CreditCardAlt
                | [<CompiledName("fa-crop")>] Crop
                | [<CompiledName("fa-crosshairs")>] Crosshairs
                | [<CompiledName("fa-css3")>] Css3
                | [<CompiledName("fa-cube")>] Cube
                | [<CompiledName("fa-cubes")>] Cubes
                | [<CompiledName("fa-cut")>] Cut
                | [<CompiledName("fa-cutlery")>] Cutlery
                | [<CompiledName("fa-dashboard")>] Dashboard
                | [<CompiledName("fa-dashcube")>] Dashcube
                | [<CompiledName("fa-database")>] Database
                | [<CompiledName("fa-deaf")>] Deaf
                | [<CompiledName("fa-deafness")>] Deafness
                | [<CompiledName("fa-dedent")>] Dedent
                | [<CompiledName("fa-delicious")>] Delicious
                | [<CompiledName("fa-desktop")>] Desktop
                | [<CompiledName("fa-deviantart")>] Deviantart
                | [<CompiledName("fa-diamond")>] Diamond
                | [<CompiledName("fa-digg")>] Digg
                | [<CompiledName("fa-dollar")>] Dollar
                | [<CompiledName("fa-dot-circle-o")>] DotCircleO
                | [<CompiledName("fa-download")>] Download
                | [<CompiledName("fa-dribbble")>] Dribbble
                | [<CompiledName("fa-drivers-license")>] DriversLicense
                | [<CompiledName("fa-drivers-license-o")>] DriversLicenseO
                | [<CompiledName("fa-dropbox")>] Dropbox
                | [<CompiledName("fa-drupal")>] Drupal
                | [<CompiledName("fa-edge")>] Edge
                | [<CompiledName("fa-edit")>] Edit
                | [<CompiledName("fa-eercast")>] Eercast
                | [<CompiledName("fa-eject")>] Eject
                | [<CompiledName("fa-ellipsis-h")>] EllipsisH
                | [<CompiledName("fa-ellipsis-v")>] EllipsisV
                | [<CompiledName("fa-empire")>] Empire
                | [<CompiledName("fa-envelope")>] Envelope
                | [<CompiledName("fa-envelope-o")>] EnvelopeO
                | [<CompiledName("fa-envelope-open")>] EnvelopeOpen
                | [<CompiledName("fa-envelope-open-o")>] EnvelopeOpenO
                | [<CompiledName("fa-envelope-square")>] EnvelopeSquare
                | [<CompiledName("fa-envira")>] Envira
                | [<CompiledName("fa-eraser")>] Eraser
                | [<CompiledName("fa-etsy")>] Etsy
                | [<CompiledName("fa-eur")>] Eur
                | [<CompiledName("fa-euro")>] Euro
                | [<CompiledName("fa-exchange")>] Exchange
                | [<CompiledName("fa-exclamation")>] Exclamation
                | [<CompiledName("fa-exclamation-circle")>] ExclamationCircle
                | [<CompiledName("fa-exclamation-triangle")>] ExclamationTriangle
                | [<CompiledName("fa-expand")>] Expand
                | [<CompiledName("fa-expeditedssl")>] Expeditedssl
                | [<CompiledName("fa-external-link")>] ExternalLink
                | [<CompiledName("fa-external-link-square")>] ExternalLinkSquare
                | [<CompiledName("fa-eye")>] Eye
                | [<CompiledName("fa-eye-slash")>] EyeSlash
                | [<CompiledName("fa-eyedropper")>] Eyedropper
                | [<CompiledName("fa-fa")>] Fa
                | [<CompiledName("fa-facebook")>] Facebook
                | [<CompiledName("fa-facebook-f")>] FacebookF
                | [<CompiledName("fa-facebook-official")>] FacebookOfficial
                | [<CompiledName("fa-facebook-square")>] FacebookSquare
                | [<CompiledName("fa-fast-backward")>] FastBackward
                | [<CompiledName("fa-fast-forward")>] FastForward
                | [<CompiledName("fa-fax")>] Fax
                | [<CompiledName("fa-feed")>] Feed
                | [<CompiledName("fa-female")>] Female
                | [<CompiledName("fa-fighter-jet")>] FighterJet
                | [<CompiledName("fa-file")>] File
                | [<CompiledName("fa-file-archive-o")>] FileArchiveO
                | [<CompiledName("fa-file-audio-o")>] FileAudioO
                | [<CompiledName("fa-file-code-o")>] FileCodeO
                | [<CompiledName("fa-file-excel-o")>] FileExcelO
                | [<CompiledName("fa-file-image-o")>] FileImageO
                | [<CompiledName("fa-file-movie-o")>] FileMovieO
                | [<CompiledName("fa-file-o")>] FileO
                | [<CompiledName("fa-file-pdf-o")>] FilePdfO
                | [<CompiledName("fa-file-photo-o")>] FilePhotoO
                | [<CompiledName("fa-file-picture-o")>] FilePictureO
                | [<CompiledName("fa-file-powerpoint-o")>] FilePowerpointO
                | [<CompiledName("fa-file-sound-o")>] FileSoundO
                | [<CompiledName("fa-file-text")>] FileText
                | [<CompiledName("fa-file-text-o")>] FileTextO
                | [<CompiledName("fa-file-video-o")>] FileVideoO
                | [<CompiledName("fa-file-word-o")>] FileWordO
                | [<CompiledName("fa-file-zip-o")>] FileZipO
                | [<CompiledName("fa-files-o")>] FilesO
                | [<CompiledName("fa-film")>] Film
                | [<CompiledName("fa-filter")>] Filter
                | [<CompiledName("fa-fire")>] Fire
                | [<CompiledName("fa-fire-extinguisher")>] FireExtinguisher
                | [<CompiledName("fa-firefox")>] Firefox
                | [<CompiledName("fa-first-order")>] FirstOrder
                | [<CompiledName("fa-flag")>] Flag
                | [<CompiledName("fa-flag-checkered")>] FlagCheckered
                | [<CompiledName("fa-flag-o")>] FlagO
                | [<CompiledName("fa-flash")>] Flash
                | [<CompiledName("fa-flask")>] Flask
                | [<CompiledName("fa-flickr")>] Flickr
                | [<CompiledName("fa-floppy-o")>] FloppyO
                | [<CompiledName("fa-folder")>] Folder
                | [<CompiledName("fa-folder-o")>] FolderO
                | [<CompiledName("fa-folder-open")>] FolderOpen
                | [<CompiledName("fa-folder-open-o")>] FolderOpenO
                | [<CompiledName("fa-font")>] Font
                | [<CompiledName("fa-font-awesome")>] FontAwesome
                | [<CompiledName("fa-fonticons")>] Fonticons
                | [<CompiledName("fa-fort-awesome")>] FortAwesome
                | [<CompiledName("fa-forumbee")>] Forumbee
                | [<CompiledName("fa-forward")>] Forward
                | [<CompiledName("fa-foursquare")>] Foursquare
                | [<CompiledName("fa-free-code-camp")>] FreeCodeCamp
                | [<CompiledName("fa-frown-o")>] FrownO
                | [<CompiledName("fa-futbol-o")>] FutbolO
                | [<CompiledName("fa-gamepad")>] Gamepad
                | [<CompiledName("fa-gavel")>] Gavel
                | [<CompiledName("fa-gbp")>] Gbp
                | [<CompiledName("fa-ge")>] Ge
                | [<CompiledName("fa-gear")>] Gear
                | [<CompiledName("fa-gears")>] Gears
                | [<CompiledName("fa-genderless")>] Genderless
                | [<CompiledName("fa-get-pocket")>] GetPocket
                | [<CompiledName("fa-gg")>] Gg
                | [<CompiledName("fa-gg-circle")>] GgCircle
                | [<CompiledName("fa-gift")>] Gift
                | [<CompiledName("fa-git")>] Git
                | [<CompiledName("fa-git-square")>] GitSquare
                | [<CompiledName("fa-github")>] Github
                | [<CompiledName("fa-github-alt")>] GithubAlt
                | [<CompiledName("fa-github-square")>] GithubSquare
                | [<CompiledName("fa-gitlab")>] Gitlab
                | [<CompiledName("fa-gittip")>] Gittip
                | [<CompiledName("fa-glass")>] Glass
                | [<CompiledName("fa-glide")>] Glide
                | [<CompiledName("fa-glide-g")>] GlideG
                | [<CompiledName("fa-globe")>] Globe
                | [<CompiledName("fa-google")>] Google
                | [<CompiledName("fa-google-plus")>] GooglePlus
                | [<CompiledName("fa-google-plus-circle")>] GooglePlusCircle
                | [<CompiledName("fa-google-plus-official")>] GooglePlusOfficial
                | [<CompiledName("fa-google-plus-square")>] GooglePlusSquare
                | [<CompiledName("fa-google-wallet")>] GoogleWallet
                | [<CompiledName("fa-graduation-cap")>] GraduationCap
                | [<CompiledName("fa-gratipay")>] Gratipay
                | [<CompiledName("fa-grav")>] Grav
                | [<CompiledName("fa-group")>] Group
                | [<CompiledName("fa-h-square")>] HSquare
                | [<CompiledName("fa-hacker-news")>] HackerNews
                | [<CompiledName("fa-hand-grab-o")>] HandGrabO
                | [<CompiledName("fa-hand-lizard-o")>] HandLizardO
                | [<CompiledName("fa-hand-o-down")>] HandODown
                | [<CompiledName("fa-hand-o-left")>] HandOLeft
                | [<CompiledName("fa-hand-o-right")>] HandORight
                | [<CompiledName("fa-hand-o-up")>] HandOUp
                | [<CompiledName("fa-hand-paper-o")>] HandPaperO
                | [<CompiledName("fa-hand-peace-o")>] HandPeaceO
                | [<CompiledName("fa-hand-pointer-o")>] HandPointerO
                | [<CompiledName("fa-hand-rock-o")>] HandRockO
                | [<CompiledName("fa-hand-scissors-o")>] HandScissorsO
                | [<CompiledName("fa-hand-spock-o")>] HandSpockO
                | [<CompiledName("fa-hand-stop-o")>] HandStopO
                | [<CompiledName("fa-handshake-o")>] HandshakeO
                | [<CompiledName("fa-hard-of-hearing")>] HardOfHearing
                | [<CompiledName("fa-hashtag")>] Hashtag
                | [<CompiledName("fa-hdd-o")>] HddO
                | [<CompiledName("fa-header")>] Header
                | [<CompiledName("fa-headphones")>] Headphones
                | [<CompiledName("fa-heart")>] Heart
                | [<CompiledName("fa-heart-o")>] HeartO
                | [<CompiledName("fa-heartbeat")>] Heartbeat
                | [<CompiledName("fa-history")>] History
                | [<CompiledName("fa-home")>] Home
                | [<CompiledName("fa-hospital-o")>] HospitalO
                | [<CompiledName("fa-hotel")>] Hotel
                | [<CompiledName("fa-hourglass")>] Hourglass
                | [<CompiledName("fa-hourglass-1")>] Hourglass1
                | [<CompiledName("fa-hourglass-2")>] Hourglass2
                | [<CompiledName("fa-hourglass-3")>] Hourglass3
                | [<CompiledName("fa-hourglass-end")>] HourglassEnd
                | [<CompiledName("fa-hourglass-half")>] HourglassHalf
                | [<CompiledName("fa-hourglass-o")>] HourglassO
                | [<CompiledName("fa-hourglass-start")>] HourglassStart
                | [<CompiledName("fa-houzz")>] Houzz
                | [<CompiledName("fa-html5")>] Html5
                | [<CompiledName("fa-i-cursor")>] ICursor
                | [<CompiledName("fa-id-badge")>] IdBadge
                | [<CompiledName("fa-id-card")>] IdCard
                | [<CompiledName("fa-id-card-o")>] IdCardO
                | [<CompiledName("fa-ils")>] Ils
                | [<CompiledName("fa-image")>] Image
                | [<CompiledName("fa-imdb")>] Imdb
                | [<CompiledName("fa-inbox")>] Inbox
                | [<CompiledName("fa-indent")>] Indent
                | [<CompiledName("fa-industry")>] Industry
                | [<CompiledName("fa-info")>] Info
                | [<CompiledName("fa-info-circle")>] InfoCircle
                | [<CompiledName("fa-inr")>] Inr
                | [<CompiledName("fa-instagram")>] Instagram
                | [<CompiledName("fa-institution")>] Institution
                | [<CompiledName("fa-internet-explorer")>] InternetExplorer
                | [<CompiledName("fa-intersex")>] Intersex
                | [<CompiledName("fa-ioxhost")>] Ioxhost
                | [<CompiledName("fa-italic")>] Italic
                | [<CompiledName("fa-joomla")>] Joomla
                | [<CompiledName("fa-jpy")>] Jpy
                | [<CompiledName("fa-jsfiddle")>] Jsfiddle
                | [<CompiledName("fa-key")>] Key
                | [<CompiledName("fa-keyboard-o")>] KeyboardO
                | [<CompiledName("fa-krw")>] Krw
                | [<CompiledName("fa-language")>] Language
                | [<CompiledName("fa-laptop")>] Laptop
                | [<CompiledName("fa-lastfm")>] Lastfm
                | [<CompiledName("fa-lastfm-square")>] LastfmSquare
                | [<CompiledName("fa-leaf")>] Leaf
                | [<CompiledName("fa-leanpub")>] Leanpub
                | [<CompiledName("fa-legal")>] Legal
                | [<CompiledName("fa-lemon-o")>] LemonO
                | [<CompiledName("fa-level-down")>] LevelDown
                | [<CompiledName("fa-level-up")>] LevelUp
                | [<CompiledName("fa-life-bouy")>] LifeBouy
                | [<CompiledName("fa-life-buoy")>] LifeBuoy
                | [<CompiledName("fa-life-ring")>] LifeRing
                | [<CompiledName("fa-life-saver")>] LifeSaver
                | [<CompiledName("fa-lightbulb-o")>] LightbulbO
                | [<CompiledName("fa-line-chart")>] LineChart
                | [<CompiledName("fa-link")>] Link
                | [<CompiledName("fa-linkedin")>] Linkedin
                | [<CompiledName("fa-linkedin-square")>] LinkedinSquare
                | [<CompiledName("fa-linode")>] Linode
                | [<CompiledName("fa-linux")>] Linux
                | [<CompiledName("fa-list")>] List
                | [<CompiledName("fa-list-alt")>] ListAlt
                | [<CompiledName("fa-list-ol")>] ListOl
                | [<CompiledName("fa-list-ul")>] ListUl
                | [<CompiledName("fa-location-arrow")>] LocationArrow
                | [<CompiledName("fa-lock")>] Lock
                | [<CompiledName("fa-long-arrow-down")>] LongArrowDown
                | [<CompiledName("fa-long-arrow-left")>] LongArrowLeft
                | [<CompiledName("fa-long-arrow-right")>] LongArrowRight
                | [<CompiledName("fa-long-arrow-up")>] LongArrowUp
                | [<CompiledName("fa-low-vision")>] LowVision
                | [<CompiledName("fa-magic")>] Magic
                | [<CompiledName("fa-magnet")>] Magnet
                | [<CompiledName("fa-mail-forward")>] MailForward
                | [<CompiledName("fa-mail-reply")>] MailReply
                | [<CompiledName("fa-mail-reply-all")>] MailReplyAll
                | [<CompiledName("fa-male")>] Male
                | [<CompiledName("fa-map")>] Map
                | [<CompiledName("fa-map-marker")>] MapMarker
                | [<CompiledName("fa-map-o")>] MapO
                | [<CompiledName("fa-map-pin")>] MapPin
                | [<CompiledName("fa-map-signs")>] MapSigns
                | [<CompiledName("fa-mars")>] Mars
                | [<CompiledName("fa-mars-double")>] MarsDouble
                | [<CompiledName("fa-mars-stroke")>] MarsStroke
                | [<CompiledName("fa-mars-stroke-h")>] MarsStrokeH
                | [<CompiledName("fa-mars-stroke-v")>] MarsStrokeV
                | [<CompiledName("fa-maxcdn")>] Maxcdn
                | [<CompiledName("fa-meanpath")>] Meanpath
                | [<CompiledName("fa-medium")>] Medium
                | [<CompiledName("fa-medkit")>] Medkit
                | [<CompiledName("fa-meetup")>] Meetup
                | [<CompiledName("fa-meh-o")>] MehO
                | [<CompiledName("fa-mercury")>] Mercury
                | [<CompiledName("fa-microchip")>] Microchip
                | [<CompiledName("fa-microphone")>] Microphone
                | [<CompiledName("fa-microphone-slash")>] MicrophoneSlash
                | [<CompiledName("fa-minus")>] Minus
                | [<CompiledName("fa-minus-circle")>] MinusCircle
                | [<CompiledName("fa-minus-square")>] MinusSquare
                | [<CompiledName("fa-minus-square-o")>] MinusSquareO
                | [<CompiledName("fa-mixcloud")>] Mixcloud
                | [<CompiledName("fa-mobile")>] Mobile
                | [<CompiledName("fa-mobile-phone")>] MobilePhone
                | [<CompiledName("fa-modx")>] Modx
                | [<CompiledName("fa-money")>] Money
                | [<CompiledName("fa-moon-o")>] MoonO
                | [<CompiledName("fa-mortar-board")>] MortarBoard
                | [<CompiledName("fa-motorcycle")>] Motorcycle
                | [<CompiledName("fa-mouse-pointer")>] MousePointer
                | [<CompiledName("fa-music")>] Music
                | [<CompiledName("fa-navicon")>] Navicon
                | [<CompiledName("fa-neuter")>] Neuter
                | [<CompiledName("fa-newspaper-o")>] NewspaperO
                | [<CompiledName("fa-object-group")>] ObjectGroup
                | [<CompiledName("fa-object-ungroup")>] ObjectUngroup
                | [<CompiledName("fa-odnoklassniki")>] Odnoklassniki
                | [<CompiledName("fa-odnoklassniki-square")>] OdnoklassnikiSquare
                | [<CompiledName("fa-opencart")>] Opencart
                | [<CompiledName("fa-openid")>] Openid
                | [<CompiledName("fa-opera")>] Opera
                | [<CompiledName("fa-optin-monster")>] OptinMonster
                | [<CompiledName("fa-outdent")>] Outdent
                | [<CompiledName("fa-pagelines")>] Pagelines
                | [<CompiledName("fa-paint-brush")>] PaintBrush
                | [<CompiledName("fa-paper-plane")>] PaperPlane
                | [<CompiledName("fa-paper-plane-o")>] PaperPlaneO
                | [<CompiledName("fa-paperclip")>] Paperclip
                | [<CompiledName("fa-paragraph")>] Paragraph
                | [<CompiledName("fa-paste")>] Paste
                | [<CompiledName("fa-pause")>] Pause
                | [<CompiledName("fa-pause-circle")>] PauseCircle
                | [<CompiledName("fa-pause-circle-o")>] PauseCircleO
                | [<CompiledName("fa-paw")>] Paw
                | [<CompiledName("fa-paypal")>] Paypal
                | [<CompiledName("fa-pencil")>] Pencil
                | [<CompiledName("fa-pencil-square")>] PencilSquare
                | [<CompiledName("fa-pencil-square-o")>] PencilSquareO
                | [<CompiledName("fa-percent")>] Percent
                | [<CompiledName("fa-phone")>] Phone
                | [<CompiledName("fa-phone-square")>] PhoneSquare
                | [<CompiledName("fa-photo")>] Photo
                | [<CompiledName("fa-picture-o")>] PictureO
                | [<CompiledName("fa-pie-chart")>] PieChart
                | [<CompiledName("fa-pied-piper")>] PiedPiper
                | [<CompiledName("fa-pied-piper-alt")>] PiedPiperAlt
                | [<CompiledName("fa-pied-piper-pp")>] PiedPiperPp
                | [<CompiledName("fa-pinterest")>] Pinterest
                | [<CompiledName("fa-pinterest-p")>] PinterestP
                | [<CompiledName("fa-pinterest-square")>] PinterestSquare
                | [<CompiledName("fa-plane")>] Plane
                | [<CompiledName("fa-play")>] Play
                | [<CompiledName("fa-play-circle")>] PlayCircle
                | [<CompiledName("fa-play-circle-o")>] PlayCircleO
                | [<CompiledName("fa-plug")>] Plug
                | [<CompiledName("fa-plus")>] Plus
                | [<CompiledName("fa-plus-circle")>] PlusCircle
                | [<CompiledName("fa-plus-square")>] PlusSquare
                | [<CompiledName("fa-plus-square-o")>] PlusSquareO
                | [<CompiledName("fa-podcast")>] Podcast
                | [<CompiledName("fa-power-off")>] PowerOff
                | [<CompiledName("fa-print")>] Print
                | [<CompiledName("fa-product-hunt")>] ProductHunt
                | [<CompiledName("fa-puzzle-piece")>] PuzzlePiece
                | [<CompiledName("fa-qq")>] Qq
                | [<CompiledName("fa-qrcode")>] Qrcode
                | [<CompiledName("fa-question")>] Question
                | [<CompiledName("fa-question-circle")>] QuestionCircle
                | [<CompiledName("fa-question-circle-o")>] QuestionCircleO
                | [<CompiledName("fa-quora")>] Quora
                | [<CompiledName("fa-quote-left")>] QuoteLeft
                | [<CompiledName("fa-quote-right")>] QuoteRight
                | [<CompiledName("fa-ra")>] Ra
                | [<CompiledName("fa-random")>] Random
                | [<CompiledName("fa-ravelry")>] Ravelry
                | [<CompiledName("fa-rebel")>] Rebel
                | [<CompiledName("fa-recycle")>] Recycle
                | [<CompiledName("fa-reddit")>] Reddit
                | [<CompiledName("fa-reddit-alien")>] RedditAlien
                | [<CompiledName("fa-reddit-square")>] RedditSquare
                | [<CompiledName("fa-refresh")>] Refresh
                | [<CompiledName("fa-registered")>] Registered
                | [<CompiledName("fa-remove")>] Remove
                | [<CompiledName("fa-renren")>] Renren
                | [<CompiledName("fa-reorder")>] Reorder
                | [<CompiledName("fa-repeat")>] Repeat
                | [<CompiledName("fa-reply")>] Reply
                | [<CompiledName("fa-reply-all")>] ReplyAll
                | [<CompiledName("fa-resistance")>] Resistance
                | [<CompiledName("fa-retweet")>] Retweet
                | [<CompiledName("fa-rmb")>] Rmb
                | [<CompiledName("fa-road")>] Road
                | [<CompiledName("fa-rocket")>] Rocket
                | [<CompiledName("fa-rotate-left")>] RotateLeft
                | [<CompiledName("fa-rotate-right")>] RotateRight
                | [<CompiledName("fa-rouble")>] Rouble
                | [<CompiledName("fa-rss")>] Rss
                | [<CompiledName("fa-rss-square")>] RssSquare
                | [<CompiledName("fa-rub")>] Rub
                | [<CompiledName("fa-ruble")>] Ruble
                | [<CompiledName("fa-rupee")>] Rupee
                | [<CompiledName("fa-s15")>] S15
                | [<CompiledName("fa-safari")>] Safari
                | [<CompiledName("fa-save")>] Save
                | [<CompiledName("fa-scissors")>] Scissors
                | [<CompiledName("fa-scribd")>] Scribd
                | [<CompiledName("fa-search")>] Search
                | [<CompiledName("fa-search-minus")>] SearchMinus
                | [<CompiledName("fa-search-plus")>] SearchPlus
                | [<CompiledName("fa-sellsy")>] Sellsy
                | [<CompiledName("fa-send")>] Send
                | [<CompiledName("fa-send-o")>] SendO
                | [<CompiledName("fa-server")>] Server
                | [<CompiledName("fa-share")>] Share
                | [<CompiledName("fa-share-alt")>] ShareAlt
                | [<CompiledName("fa-share-alt-square")>] ShareAltSquare
                | [<CompiledName("fa-share-square")>] ShareSquare
                | [<CompiledName("fa-share-square-o")>] ShareSquareO
                | [<CompiledName("fa-shekel")>] Shekel
                | [<CompiledName("fa-sheqel")>] Sheqel
                | [<CompiledName("fa-shield")>] Shield
                | [<CompiledName("fa-ship")>] Ship
                | [<CompiledName("fa-shirtsinbulk")>] Shirtsinbulk
                | [<CompiledName("fa-shopping-bag")>] ShoppingBag
                | [<CompiledName("fa-shopping-basket")>] ShoppingBasket
                | [<CompiledName("fa-shopping-cart")>] ShoppingCart
                | [<CompiledName("fa-shower")>] Shower
                | [<CompiledName("fa-sign-in")>] SignIn
                | [<CompiledName("fa-sign-language")>] SignLanguage
                | [<CompiledName("fa-sign-out")>] SignOut
                | [<CompiledName("fa-signal")>] Signal
                | [<CompiledName("fa-signing")>] Signing
                | [<CompiledName("fa-simplybuilt")>] Simplybuilt
                | [<CompiledName("fa-sitemap")>] Sitemap
                | [<CompiledName("fa-skyatlas")>] Skyatlas
                | [<CompiledName("fa-skype")>] Skype
                | [<CompiledName("fa-slack")>] Slack
                | [<CompiledName("fa-sliders")>] Sliders
                | [<CompiledName("fa-slideshare")>] Slideshare
                | [<CompiledName("fa-smile-o")>] SmileO
                | [<CompiledName("fa-snapchat")>] Snapchat
                | [<CompiledName("fa-snapchat-ghost")>] SnapchatGhost
                | [<CompiledName("fa-snapchat-square")>] SnapchatSquare
                | [<CompiledName("fa-snowflake-o")>] SnowflakeO
                | [<CompiledName("fa-soccer-ball-o")>] SoccerBallO
                | [<CompiledName("fa-sort")>] Sort
                | [<CompiledName("fa-sort-alpha-asc")>] SortAlphaAsc
                | [<CompiledName("fa-sort-alpha-desc")>] SortAlphaDesc
                | [<CompiledName("fa-sort-amount-asc")>] SortAmountAsc
                | [<CompiledName("fa-sort-amount-desc")>] SortAmountDesc
                | [<CompiledName("fa-sort-asc")>] SortAsc
                | [<CompiledName("fa-sort-desc")>] SortDesc
                | [<CompiledName("fa-sort-down")>] SortDown
                | [<CompiledName("fa-sort-numeric-asc")>] SortNumericAsc
                | [<CompiledName("fa-sort-numeric-desc")>] SortNumericDesc
                | [<CompiledName("fa-sort-up")>] SortUp
                | [<CompiledName("fa-soundcloud")>] Soundcloud
                | [<CompiledName("fa-space-shuttle")>] SpaceShuttle
                | [<CompiledName("fa-spinner")>] Spinner
                | [<CompiledName("fa-spoon")>] Spoon
                | [<CompiledName("fa-spotify")>] Spotify
                | [<CompiledName("fa-square")>] Square
                | [<CompiledName("fa-square-o")>] SquareO
                | [<CompiledName("fa-stack-exchange")>] StackExchange
                | [<CompiledName("fa-stack-overflow")>] StackOverflow
                | [<CompiledName("fa-star")>] Star
                | [<CompiledName("fa-star-half")>] StarHalf
                | [<CompiledName("fa-star-half-empty")>] StarHalfEmpty
                | [<CompiledName("fa-star-half-full")>] StarHalfFull
                | [<CompiledName("fa-star-half-o")>] StarHalfO
                | [<CompiledName("fa-star-o")>] StarO
                | [<CompiledName("fa-steam")>] Steam
                | [<CompiledName("fa-steam-square")>] SteamSquare
                | [<CompiledName("fa-step-backward")>] StepBackward
                | [<CompiledName("fa-step-forward")>] StepForward
                | [<CompiledName("fa-stethoscope")>] Stethoscope
                | [<CompiledName("fa-sticky-note")>] StickyNote
                | [<CompiledName("fa-sticky-note-o")>] StickyNoteO
                | [<CompiledName("fa-stop")>] Stop
                | [<CompiledName("fa-stop-circle")>] StopCircle
                | [<CompiledName("fa-stop-circle-o")>] StopCircleO
                | [<CompiledName("fa-street-view")>] StreetView
                | [<CompiledName("fa-strikethrough")>] Strikethrough
                | [<CompiledName("fa-stumbleupon")>] Stumbleupon
                | [<CompiledName("fa-stumbleupon-circle")>] StumbleuponCircle
                | [<CompiledName("fa-subscript")>] Subscript
                | [<CompiledName("fa-subway")>] Subway
                | [<CompiledName("fa-suitcase")>] Suitcase
                | [<CompiledName("fa-sun-o")>] SunO
                | [<CompiledName("fa-superpowers")>] Superpowers
                | [<CompiledName("fa-superscript")>] Superscript
                | [<CompiledName("fa-support")>] Support
                | [<CompiledName("fa-table")>] Table
                | [<CompiledName("fa-tablet")>] Tablet
                | [<CompiledName("fa-tachometer")>] Tachometer
                | [<CompiledName("fa-tag")>] Tag
                // | [<CompiledName("fa-tags")>] Tags
                | [<CompiledName("fa-tasks")>] Tasks
                | [<CompiledName("fa-taxi")>] Taxi
                | [<CompiledName("fa-telegram")>] Telegram
                | [<CompiledName("fa-television")>] Television
                | [<CompiledName("fa-tencent-weibo")>] TencentWeibo
                | [<CompiledName("fa-terminal")>] Terminal
                | [<CompiledName("fa-text-height")>] TextHeight
                | [<CompiledName("fa-text-width")>] TextWidth
                | [<CompiledName("fa-th")>] Th
                | [<CompiledName("fa-th-large")>] ThLarge
                | [<CompiledName("fa-th-list")>] ThList
                | [<CompiledName("fa-themeisle")>] Themeisle
                | [<CompiledName("fa-thermometer")>] Thermometer
                | [<CompiledName("fa-thermometer-0")>] Thermometer0
                | [<CompiledName("fa-thermometer-1")>] Thermometer1
                | [<CompiledName("fa-thermometer-2")>] Thermometer2
                | [<CompiledName("fa-thermometer-3")>] Thermometer3
                | [<CompiledName("fa-thermometer-4")>] Thermometer4
                | [<CompiledName("fa-thermometer-empty")>] ThermometerEmpty
                | [<CompiledName("fa-thermometer-full")>] ThermometerFull
                | [<CompiledName("fa-thermometer-half")>] ThermometerHalf
                | [<CompiledName("fa-thermometer-quarter")>] ThermometerQuarter
                | [<CompiledName("fa-thermometer-three-quarters")>] ThermometerThreeQuarters
                | [<CompiledName("fa-thumb-tack")>] ThumbTack
                | [<CompiledName("fa-thumbs-down")>] ThumbsDown
                | [<CompiledName("fa-thumbs-o-down")>] ThumbsODown
                | [<CompiledName("fa-thumbs-o-up")>] ThumbsOUp
                | [<CompiledName("fa-thumbs-up")>] ThumbsUp
                | [<CompiledName("fa-ticket")>] Ticket
                | [<CompiledName("fa-times")>] Times
                | [<CompiledName("fa-times-circle")>] TimesCircle
                | [<CompiledName("fa-times-circle-o")>] TimesCircleO
                | [<CompiledName("fa-times-rectangle")>] TimesRectangle
                | [<CompiledName("fa-times-rectangle-o")>] TimesRectangleO
                | [<CompiledName("fa-tint")>] Tint
                | [<CompiledName("fa-toggle-down")>] ToggleDown
                | [<CompiledName("fa-toggle-left")>] ToggleLeft
                | [<CompiledName("fa-toggle-off")>] ToggleOff
                | [<CompiledName("fa-toggle-on")>] ToggleOn
                | [<CompiledName("fa-toggle-right")>] ToggleRight
                | [<CompiledName("fa-toggle-up")>] ToggleUp
                | [<CompiledName("fa-trademark")>] Trademark
                | [<CompiledName("fa-train")>] Train
                | [<CompiledName("fa-transgender")>] Transgender
                | [<CompiledName("fa-transgender-alt")>] TransgenderAlt
                | [<CompiledName("fa-trash")>] Trash
                | [<CompiledName("fa-trash-o")>] TrashO
                | [<CompiledName("fa-tree")>] Tree
                | [<CompiledName("fa-trello")>] Trello
                | [<CompiledName("fa-tripadvisor")>] Tripadvisor
                | [<CompiledName("fa-trophy")>] Trophy
                | [<CompiledName("fa-truck")>] Truck
                | [<CompiledName("fa-try")>] Try
                | [<CompiledName("fa-tty")>] Tty
                | [<CompiledName("fa-tumblr")>] Tumblr
                | [<CompiledName("fa-tumblr-square")>] TumblrSquare
                | [<CompiledName("fa-turkish-lira")>] TurkishLira
                | [<CompiledName("fa-tv")>] Tv
                | [<CompiledName("fa-twitch")>] Twitch
                | [<CompiledName("fa-twitter")>] Twitter
                | [<CompiledName("fa-twitter-square")>] TwitterSquare
                | [<CompiledName("fa-umbrella")>] Umbrella
                | [<CompiledName("fa-underline")>] Underline
                | [<CompiledName("fa-undo")>] Undo
                | [<CompiledName("fa-universal-access")>] UniversalAccess
                | [<CompiledName("fa-university")>] University
                | [<CompiledName("fa-unlink")>] Unlink
                | [<CompiledName("fa-unlock")>] Unlock
                | [<CompiledName("fa-unlock-alt")>] UnlockAlt
                | [<CompiledName("fa-unsorted")>] Unsorted
                | [<CompiledName("fa-upload")>] Upload
                | [<CompiledName("fa-usb")>] Usb
                | [<CompiledName("fa-usd")>] Usd
                | [<CompiledName("fa-user")>] User
                | [<CompiledName("fa-user-circle")>] UserCircle
                | [<CompiledName("fa-user-circle-o")>] UserCircleO
                | [<CompiledName("fa-user-md")>] UserMd
                | [<CompiledName("fa-user-o")>] UserO
                | [<CompiledName("fa-user-plus")>] UserPlus
                | [<CompiledName("fa-user-secret")>] UserSecret
                | [<CompiledName("fa-user-times")>] UserTimes
                | [<CompiledName("fa-users")>] Users
                | [<CompiledName("fa-vcard")>] Vcard
                | [<CompiledName("fa-vcard-o")>] VcardO
                | [<CompiledName("fa-venus")>] Venus
                | [<CompiledName("fa-venus-double")>] VenusDouble
                | [<CompiledName("fa-venus-mars")>] VenusMars
                | [<CompiledName("fa-viacoin")>] Viacoin
                | [<CompiledName("fa-viadeo")>] Viadeo
                | [<CompiledName("fa-viadeo-square")>] ViadeoSquare
                | [<CompiledName("fa-video-camera")>] VideoCamera
                | [<CompiledName("fa-vimeo")>] Vimeo
                | [<CompiledName("fa-vimeo-square")>] VimeoSquare
                | [<CompiledName("fa-vine")>] Vine
                | [<CompiledName("fa-vk")>] Vk
                | [<CompiledName("fa-volume-control-phone")>] VolumeControlPhone
                | [<CompiledName("fa-volume-down")>] VolumeDown
                | [<CompiledName("fa-volume-off")>] VolumeOff
                | [<CompiledName("fa-volume-up")>] VolumeUp
                | [<CompiledName("fa-warning")>] Warning
                | [<CompiledName("fa-wechat")>] Wechat
                | [<CompiledName("fa-weibo")>] Weibo
                | [<CompiledName("fa-weixin")>] Weixin
                | [<CompiledName("fa-whatsapp")>] Whatsapp
                | [<CompiledName("fa-wheelchair")>] Wheelchair
                | [<CompiledName("fa-wheelchair-alt")>] WheelchairAlt
                | [<CompiledName("fa-wifi")>] Wifi
                | [<CompiledName("fa-wikipedia-w")>] WikipediaW
                | [<CompiledName("fa-window-close")>] WindowClose
                | [<CompiledName("fa-window-close-o")>] WindowCloseO
                | [<CompiledName("fa-window-maximize")>] WindowMaximize
                | [<CompiledName("fa-window-minimize")>] WindowMinimize
                | [<CompiledName("fa-window-restore")>] WindowRestore
                | [<CompiledName("fa-windows")>] Windows
                | [<CompiledName("fa-won")>] Won
                | [<CompiledName("fa-wordpress")>] Wordpress
                | [<CompiledName("fa-wpbeginner")>] Wpbeginner
                | [<CompiledName("fa-wpexplorer")>] Wpexplorer
                | [<CompiledName("fa-wpforms")>] Wpforms
                | [<CompiledName("fa-wrench")>] Wrench
                | [<CompiledName("fa-xing")>] Xing
                | [<CompiledName("fa-xing-square")>] XingSquare
                | [<CompiledName("fa-y-combinator")>] YCombinator
                | [<CompiledName("fa-y-combinator-square")>] YCombinatorSquare
                | [<CompiledName("fa-yahoo")>] Yahoo
                | [<CompiledName("fa-yc")>] Yc
                | [<CompiledName("fa-yc-square")>] YcSquare
                | [<CompiledName("fa-yelp")>] Yelp
                | [<CompiledName("fa-yen")>] Yen
                | [<CompiledName("fa-yoast")>] Yoast
                | [<CompiledName("fa-youtube")>] Youtube
                | [<CompiledName("fa-youtube-play")>] YoutubePlay
                | [<CompiledName("fa-youtube-square")>] YoutubeSquare
                interface  IFontAwesomeIcon

            let inline Tags<'T> = unbox<IFontAwesomeIcon> "fa-tags"
            let inline ``500px``<'T> = unbox<IFontAwesomeIcon> "fa-500px"

            // Work around if an icon class is not defined
            let inline Custom (iconClass: string) = unbox<IFontAwesomeIcon> iconClass

        module Classes =
            module IconSizes =
                let [<Literal>] FaLarge = "fa-lg"
                let [<Literal>] Fa2x    = "fa-2x"
                let [<Literal>] Fa3x    = "fa-3x"
                let [<Literal>] Fa4x    = "fa-4x"
                let [<Literal>] Fa5x    = "fa-5x"
                let [<Literal>] Fw      = "fa-fw"

            module ChildSizes =
                let [<Literal>] FaStack1x = "fa-stack-1x"
                let [<Literal>] FaStack2x = "fa-stack-2x"

            module Borders =
                let [<Literal>] Border = "fa-border"

            module Pulls =
                let [<Literal>] PullRight = "fa-pull-right"
                let [<Literal>] PullLeft = "fa-pull-left"

            module Animations =
                let [<Literal>] Spin = "fa-spin"
                let [<Literal>] Pulse = "fa-pulse"
            module Rotations =
                let [<Literal>] Rotate90 = "fa-rotate-90"
                let [<Literal>] Rotate180 = "fa-rotate-180"
                let [<Literal>] Rotate270 = "fa-rotate-270"


            module Flips =
                let [<Literal>] Horizontal = "fa-flip-horizontal"
                let [<Literal>] Vertical = "fa-flip-vertical"

            module Colors =
                let [<Literal>] Inverse = "fa-inverse"

            module StackChildSizes =
                let [<Literal>] Fa1x = "fa-stack-1x"
                let [<Literal>] Fa2x = "fa-stack-2x"

        module Types =
            open I
            type [<Fable.Core.CompileAsArray>] IIconSize =
                | FaLarge
                | Fa2x
                | Fa3x
                | Fa4x
                | Fa5x
                | Fw

            type [<Fable.Core.CompileAsArray>] IBorder = FaBorder

            type [<Fable.Core.CompileAsArray>] IPull =
                | PullLeft
                | PullRight

            type [<Fable.Core.CompileAsArray>] IAnimation =
                | Spin
                | Pulse

            type [<Fable.Core.CompileAsArray>] IRotation =
                | Rotate90
                | Rotate180
                | Rotate270

            type [<Fable.Core.CompileAsArray>] IFLip =
                | Horizontal
                | Vertical

            type [<Fable.Core.CompileAsArray>] IColor =
                | Inverse

            type [<Fable.Core.CompileAsArray>] IStackChildSize =
                | FaStack1x
                | FaStack2x

            type [<Fable.Core.CompileAsArray>] IconOption =
                | Size          of IIconSize
                | Border        of IBorder
                | Pull          of IPull
                | Rotation      of IRotation
                | Flip          of IFLip
                | Color         of IColor
                | Icon          of IFontAwesomeIcon
                | Animation     of IAnimation
                | IsLi

            type [<Fable.Core.CompileAsArray>] StackParentOption =
                | ParentSize of IIconSize

            type [<Fable.Core.CompileAsArray>] StackChildOption =
                | ChildSize       of IStackChildSize
                | ChildColor      of IColor
                | ChildIcon       of IFontAwesomeIcon

            let ofSize =
                function
                | FaLarge   -> Classes.IconSizes.FaLarge
                | Fa2x      -> Classes.IconSizes.Fa2x
                | Fa3x      -> Classes.IconSizes.Fa3x
                | Fa4x      -> Classes.IconSizes.Fa4x
                | Fa5x      -> Classes.IconSizes.Fa5x
                | Fw        -> Classes.IconSizes.Fw

            let ofChildSize =
                function
                | FaStack1x   -> Classes.ChildSizes.FaStack1x
                | FaStack2x   -> Classes.ChildSizes.FaStack2x

            let ofBorder =
                function
                | FaBorder -> Classes.Borders.Border

            let ofPull =
                function
                | PullLeft  -> Classes.Pulls.PullLeft
                | PullRight -> Classes.Pulls.PullRight

            let ofAnimation =
                function
                | Spin      -> Classes.Animations.Spin
                | Pulse     -> Classes.Animations.Pulse
            let ofRotation =
                function
                | Rotate90  -> Classes.Rotations.Rotate90
                | Rotate180 -> Classes.Rotations.Rotate180
                | Rotate270 -> Classes.Rotations.Rotate270

            let ofFlip =
                function
                | Horizontal    -> Classes.Flips.Horizontal
                | Vertical      -> Classes.Flips.Vertical

            let ofColor =
                function
                | Inverse       -> Classes.Colors.Inverse

            type [<Fable.Core.CompileAsArray>] IconOptions =
                {
                    Icon        : string option
                    Size        : string option
                    Border      : string option
                    Pull        : string option
                    Animation   : string option
                    Rotation    : string option
                    Flip        : string option
                    Color       : string option
                    IsLi        : bool
                }
            let inline defaultIconOptions() =
                        {
                            Icon        = None
                            Size        = None
                            Border      = None
                            Pull        = None
                            Animation   = None
                            Rotation    = None
                            Flip        = None
                            Color       = None
                            IsLi        = false
                        }

            type [<Fable.Core.CompileAsArray>] StackParentOptions =
                {
                    Size  : string option
                }
            let inline defaultStackParentOptions() =
                        {
                            Size    = None
                        }

            type [<Fable.Core.CompileAsArray>] StackChildOptions =
                {
                    Size  : string option
                    Color : string option
                    Icon  : string option
                }
            let inline defaultStackChildOptions() =
                        {
                            Size    = None
                            Color   = None
                            Icon    = None
                        }
        open Types
        let icon (i: I.IFontAwesomeIcon) = IconOption.Icon i
        let faLg                         = IconOption.Size FaLarge
        let fa2x                         = IconOption.Size Fa2x
        let fa3x                         = IconOption.Size Fa3x
        let fa4x                         = IconOption.Size Fa4x
        let fa5x                         = IconOption.Size Fa5x
        let fw                           = IconOption.Size Fw
        let border                       = IconOption.Border FaBorder
        let pullLeft                     = IconOption.Pull PullLeft
        let pullRight                    = IconOption.Pull PullRight
        let rotate90                     = IconOption.Rotation Rotate90
        let rotate180                    = IconOption.Rotation Rotate180
        let rotate270                    = IconOption.Rotation Rotate270
        let spin                         = IconOption.Animation Spin
        let pulse                        = IconOption.Animation Pulse
        let flipHorizontal               = IconOption.Flip Horizontal
        let flipVertical                 = IconOption.Flip Vertical
        let colorInverse                 = IconOption.Color Inverse
        let isLi                         = IconOption.IsLi

        //Stack Child Functions
        module Child =
            let faStack2x                       = StackChildOption.ChildSize FaStack2x
            let faStack1x                       = StackChildOption.ChildSize FaStack1x
            let colorInverse                    = StackChildOption.ChildColor Inverse
            let icon (i: I.IFontAwesomeIcon)    = StackChildOption.ChildIcon i

        //Stack Parent Functions
        module Parent =
            let faLg      = StackParentOption.ParentSize FaLarge
    module Icon =
        open Fa.Types
        let stackChild (faOptions: StackChildOption list ) =
            let parseOptions (result: StackChildOptions) (option: StackChildOption) =
                    match option with
                    | ChildSize s               -> { result with Size   = ofChildSize  s       |> Some }
                    | ChildColor c              -> { result with Color  = ofColor c            |> Some }
                    | ChildIcon faIcon          -> { result with Icon   = unbox<string> faIcon |> Some }

            let opts = faOptions |> List.fold parseOptions (defaultStackChildOptions())
            i [Helpers.classes "fa" [opts.Icon; opts.Size; opts.Color] []] []

        let stackParent (faOptions: StackParentOption list) children =
            let parseOptions (result: StackParentOptions) (option: StackParentOption) =
                    match option with
                    | ParentSize s        -> { result with Size   = ofSize s  |> Some }

            let opts = faOptions |> List.fold parseOptions (defaultStackParentOptions())
            span [Helpers.classes "fa-stack" [opts.Size] []] children

        let toIconOptions (faOptions: IconOption list) =
            let parseOptions (result: IconOptions) (option: IconOption) =
                    match option with
                    | Size s        -> { result with Size       = ofSize s              |> Some }
                    | Border b      -> { result with Border     = ofBorder b            |> Some }
                    | Pull p        -> { result with Pull       = ofPull p              |> Some }
                    | Icon faIcon   -> { result with Icon       = unbox<string> faIcon  |> Some }
                    | Rotation r    -> { result with Rotation   = ofRotation r          |> Some }
                    | Flip f        -> { result with Rotation   = ofFlip f              |> Some }
                    | Color i       -> { result with Color      = ofColor i             |> Some }
                    | Animation a   -> { result with Animation  = ofAnimation a         |> Some }
                    | IsLi          -> { result with IsLi       = false }

            faOptions |> List.fold parseOptions (defaultIconOptions())


        //Logic used to display one icon alone or as one item in an unordered list:
        let displayIcon baseClass  (opts: IconOptions) =
            i [ Helpers.classes baseClass
                    [ opts.Icon; opts.Size; opts.Border
                      opts.Pull; opts.Animation; opts.Rotation
                      opts.Flip; opts.Color ] [] ] []

        let faIcon (options : Fulma.Elements.Icon.Option list) (faOptions: IconOption list) =
            let opts = toIconOptions faOptions
            Icon.icon options
                [ displayIcon "fa " opts ]

        let fa_ul (options: GenericOption list) children =
            let opts = genericParse options
            let classes = Helpers.classes "fa-ul" [opts.CustomClass] []
            ul (classes::opts.Props) children
