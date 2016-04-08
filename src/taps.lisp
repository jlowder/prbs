(in-package :cl-user)

(defpackage prbs.taps
  (:use :cl)
  (:export :taps))

(in-package :prbs.taps)

(defparameter *lfsr2*
  (list
   2 '(2 1)
   3 '(3 2)
   4 '(4 3)
   5 '(5 3)
   6 '(6 5)
   7 '(7 6)
   9 '(9 5)
   10 '(10 7)
   11 '(11 9)
   15 '(15 14)
   17 '(17 14)
   18 '(18 11)
   20 '(20 17)
   21 '(21 19)
   22 '(22 21)
   23 '(23 18)
   25 '(25 22)
   28 '(28 25)
   29 '(29 27)
   31 '(31 28)
   33 '(33 20)
   35 '(35 33)
   36 '(36 25)
   39 '(39 35)
   41 '(41 38)
   47 '(47 42)
   49 '(49 40)
   52 '(52 49)
   55 '(55 31)
   57 '(57 50)
   58 '(58 39)
   60 '(60 59)
   63 '(63 62)
   65 '(65 47)
   68 '(68 59)
   71 '(71 65)
   73 '(73 48)
   79 '(79 70)
   81 '(81 77)
   84 '(84 71)
   87 '(87 74)
   89 '(89 51)
   93 '(93 91)
   94 '(94 73)
   95 '(95 84)
   97 '(97 91)
   98 '(98 87)
   100 '(100 63)
   103 '(103 94)
   105 '(105 89)
   106 '(106 91)
   108 '(108 77)
   111 '(111 101)
   113 '(113 104)
   118 '(118 85)
   119 '(119 111)
   121 '(121 103)
   123 '(123 121)
   124 '(124 87)
   127 '(127 126)
   129 '(129 124)
   130 '(130 127)
   132 '(132 103)
   134 '(134 77)
   135 '(135 124)
   137 '(137 116)
   140 '(140 111)
   142 '(142 121)
   145 '(145 93)
   148 '(148 121)
   150 '(150 97)
   151 '(151 148)
   153 '(153 152)
   159 '(159 128)
   161 '(161 143)
   167 '(167 161)
   169 '(169 135)
   170 '(170 147)
   172 '(172 165)
   174 '(174 161)
   175 '(175 169)
   177 '(177 169)
   178 '(178 91)
   183 '(183 127)
   185 '(185 161)
   191 '(191 182)
   193 '(193 178)
   194 '(194 107)
   198 '(198 133)
   199 '(199 165)
   201 '(201 187)
   202 '(202 147)
   207 '(207 164)
   209 '(209 203)
   212 '(212 107)
   215 '(215 192)
   217 '(217 172)
   218 '(218 207)
   223 '(223 190)
   225 '(225 193)
   231 '(231 205)
   233 '(233 159)
   234 '(234 203)
   236 '(236 231)
   239 '(239 203)
   241 '(241 171)
   247 '(247 165)
   249 '(249 163)
   250 '(250 147)
   252 '(252 185)
   255 '(255 203)
   257 '(257 245)
   258 '(258 175)
   263 '(263 170)
   265 '(265 223)
   266 '(266 219)
   268 '(268 243)
   270 '(270 217)
   271 '(271 213)
   273 '(273 250)
   274 '(274 207)
   278 '(278 273)
   279 '(279 274)
   281 '(281 188)
   282 '(282 247)
   284 '(284 165)
   286 '(286 217)
   287 '(287 216)
   289 '(289 268)
   292 '(292 195)
   294 '(294 233)
   295 '(295 247)
   297 '(297 292)
   300 '(300 293)
   302 '(302 261)
   305 '(305 203)
   313 '(313 234)
   314 '(314 299)
   316 '(316 181)
   319 '(319 283)
   321 '(321 290)
   322 '(322 255)
   327 '(327 293)
   329 '(329 279)
   332 '(332 209)
   333 '(333 331)
   337 '(337 282)
   342 '(342 217)
   343 '(343 268)
   345 '(345 323)
   350 '(350 297)
   351 '(351 317)
   353 '(353 284)
   359 '(359 291)
   362 '(362 299)
   364 '(364 297)
   366 '(366 337)
   367 '(367 346)
   369 '(369 278)
   370 '(370 231)
   375 '(375 359)
   377 '(377 336)
   378 '(378 335)
   380 '(380 333)
   382 '(382 301)
   383 '(383 293)
   385 '(385 379)
   386 '(386 303)
   390 '(390 301)
   391 '(391 363)
   393 '(393 386)
   394 '(394 259)
   396 '(396 371)
   399 '(399 313)
   401 '(401 249)
   404 '(404 215)
   406 '(406 249)
   407 '(407 336)
   409 '(409 322)
   412 '(412 265)
   415 '(415 313)
   417 '(417 310)
   422 '(422 273)
   423 '(423 398)
   425 '(425 413)
   428 '(428 323)
   431 '(431 311)
   433 '(433 400)
   436 '(436 271)
   438 '(438 373)
   439 '(439 390)
   441 '(441 410)
   446 '(446 341)
   447 '(447 374)
   449 '(449 315)
   450 '(450 371)
   455 '(455 417)
   457 '(457 441)
   458 '(458 255)
   460 '(460 399)
   462 '(462 389)
   463 '(463 370)
   465 '(465 406)
   470 '(470 321)
   471 '(471 470)
   474 '(474 283)
   476 '(476 461)
   478 '(478 357)
   479 '(479 375)
   481 '(481 343)
   484 '(484 379)
   487 '(487 393)
   489 '(489 406)
   490 '(490 271)
   494 '(494 357)
   495 '(495 419)
   497 '(497 419)
   503 '(503 500)
   505 '(505 349)
   506 '(506 411)
   508 '(508 399)
   511 '(511 501)
   513 '(513 428)
   518 '(518 485)
   519 '(519 440)
   521 '(521 489)
   524 '(524 357)
   527 '(527 480)
   529 '(529 487)
   532 '(532 531)
   537 '(537 443)
   540 '(540 361)
   543 '(543 527)
   545 '(545 423)
   550 '(550 357)
   551 '(551 416)
   553 '(553 514)
   556 '(556 403)
   559 '(559 525)
   561 '(561 490)
   564 '(564 401)
   566 '(566 413)
   567 '(567 424)
   569 '(569 492)
   570 '(570 503)
   574 '(574 561)
   575 '(575 429)
   577 '(577 552)
   582 '(582 497)
   583 '(583 453)
   585 '(585 464)
   588 '(588 437)
   590 '(590 497)
   593 '(593 507)
   594 '(594 575)
   599 '(599 569)
   601 '(601 400)
   607 '(607 502)
   609 '(609 578)
   610 '(610 483)
   615 '(615 404)
   617 '(617 417)
   622 '(622 325)
   623 '(623 555)
   625 '(625 492)
   628 '(628 405)
   631 '(631 324)
   633 '(633 532)
   634 '(634 319)
   639 '(639 623)
   641 '(641 630)
   642 '(642 523)
   646 '(646 397)
   647 '(647 642)
   649 '(649 612)
   650 '(650 647)
   652 '(652 559)
   655 '(655 567)
   657 '(657 619)
   658 '(658 603)
   662 '(662 365)
   663 '(663 406)
   665 '(665 632)
   670 '(670 517)
   671 '(671 656)
   673 '(673 645)
   676 '(676 435)
   679 '(679 613)
   686 '(686 489)
   687 '(687 674)
   689 '(689 675)
   692 '(692 393)
   695 '(695 483)
   697 '(697 430)
   698 '(698 483)
   702 '(702 665)
   705 '(705 686)
   708 '(708 421)
   711 '(711 619)
   713 '(713 672)
   714 '(714 691)
   716 '(716 533)
   719 '(719 569)
   721 '(721 712)
   722 '(722 491)
   726 '(726 721)
   727 '(727 547)
   729 '(729 671)
   730 '(730 583)
   735 '(735 691)
   737 '(737 732)
   738 '(738 391)
   740 '(740 587)
   743 '(743 653)
   745 '(745 487)
   746 '(746 395)
   751 '(751 733)
   753 '(753 595)
   754 '(754 735)
   756 '(756 407)
   759 '(759 661)
   761 '(761 758)
   762 '(762 679)
   767 '(767 599)
   769 '(769 649)
   772 '(772 765)
   774 '(774 589)
   775 '(775 408)
   777 '(777 748)
   778 '(778 403)
   782 '(782 453)
   783 '(783 715)
   785 '(785 693)))

(defparameter *lfsr4*
  (list
   5 '(5 4 3 2)
   6 '(6 5 3 2)
   7 '(7 6 5 4)
   8 '(8 6 5 4)
   9 '(9 8 6 5)
   10 '(10 9 7 6)
   11 '(11 10 9 7)
   12 '(12 11 8 6)
   13 '(13 12 10 9)
   14 '(14 13 11 9)
   15 '(15 14 13 11)
   16 '(16 14 13 11)
   17 '(17 16 15 14)
   18 '(18 17 16 13)
   19 '(19 18 17 14)
   20 '(20 19 16 14)
   21 '(21 20 19 16)
   22 '(22 19 18 17)
   23 '(23 22 20 18)
   24 '(24 23 21 20)
   25 '(25 24 23 22)
   26 '(26 25 24 20)
   27 '(27 26 25 22)
   28 '(28 27 24 22)
   29 '(29 28 27 25)
   30 '(30 29 26 24)
   31 '(31 30 29 28)
   32 '(32 30 26 25)
   33 '(33 32 29 27)
   34 '(34 31 30 26)
   35 '(35 34 28 27)
   36 '(36 35 29 28)
   37 '(37 36 33 31)
   38 '(38 37 33 32)
   39 '(39 38 35 32)
   40 '(40 37 36 35)
   41 '(41 40 39 38)
   42 '(42 40 37 35)
   43 '(43 42 38 37)
   44 '(44 42 39 38)
   45 '(45 44 42 41)
   46 '(46 40 39 38)
   47 '(47 46 43 42)
   48 '(48 44 41 39)
   49 '(49 45 44 43)
   50 '(50 48 47 46)
   51 '(51 50 48 45)
   52 '(52 51 49 46)
   53 '(53 52 51 47)
   54 '(54 51 48 46)
   55 '(55 54 53 49)
   56 '(56 54 52 49)
   57 '(57 55 54 52)
   58 '(58 57 53 52)
   59 '(59 57 55 52)
   60 '(60 58 56 55)
   61 '(61 60 59 56)
   62 '(62 59 57 56)
   63 '(63 62 59 58)
   64 '(64 63 61 60)
   65 '(65 64 62 61)
   66 '(66 60 58 57)
   67 '(67 66 65 62)
   68 '(68 67 63 61)
   69 '(69 67 64 63)
   70 '(70 69 67 65)
   71 '(71 70 68 66)
   72 '(72 69 63 62)
   73 '(73 71 70 69)
   74 '(74 71 70 67)
   75 '(75 74 72 69)
   76 '(76 74 72 71)
   77 '(77 75 72 71)
   78 '(78 77 76 71)
   79 '(79 77 76 75)
   80 '(80 78 76 71)
   81 '(81 79 78 75)
   82 '(82 78 76 73)
   83 '(83 81 79 76)
   84 '(84 83 77 75)
   85 '(85 84 83 77)
   86 '(86 84 81 80)
   87 '(87 86 82 80)
   88 '(88 80 79 77)
   89 '(89 86 84 83)
   90 '(90 88 87 85)
   91 '(91 90 86 83)
   92 '(92 90 87 86)
   93 '(93 91 90 87)
   94 '(94 93 89 88)
   95 '(95 94 90 88)
   96 '(96 90 87 86)
   97 '(97 95 93 91)
   98 '(98 97 91 90)
   99 '(99 95 94 92)
   100 '(100 98 93 92)
   101 '(101 100 95 94)
   102 '(102 99 97 96)
   103 '(103 102 99 94)
   104 '(104 103 94 93)
   105 '(105 104 99 98)
   106 '(106 105 101 100)
   107 '(107 105 99 98)
   108 '(108 103 97 96)
   109 '(109 107 105 104)
   110 '(110 109 106 104)
   111 '(111 109 107 104)
   112 '(112 108 106 101)
   113 '(113 111 110 108)
   114 '(114 113 112 103)
   115 '(115 110 108 107)
   116 '(116 114 111 110)
   117 '(117 116 115 112)
   118 '(118 116 113 112)
   119 '(119 116 111 110)
   120 '(120 118 114 111)
   121 '(121 120 116 113)
   122 '(122 121 120 116)
   123 '(123 122 119 115)
   124 '(124 119 118 117)
   125 '(125 120 119 118)
   126 '(126 124 122 119)
   127 '(127 126 124 120)
   128 '(128 127 126 121)
   129 '(129 128 125 124)
   130 '(130 129 128 125)
   131 '(131 129 128 123)
   132 '(132 130 127 123)
   133 '(133 131 125 124)
   134 '(134 133 129 127)
   135 '(135 132 131 129)
   136 '(136 134 133 128)
   137 '(137 136 133 126)
   138 '(138 137 131 130)
   139 '(139 136 134 131)
   140 '(140 139 136 132)
   141 '(141 140 135 128)
   142 '(142 141 139 132)
   143 '(143 141 140 138)
   144 '(144 142 140 137)
   145 '(145 144 140 139)
   146 '(146 144 143 141)
   147 '(147 145 143 136)
   148 '(148 145 143 141)
   149 '(149 142 140 139)
   150 '(150 148 147 142)
   151 '(151 150 149 148)
   152 '(152 150 149 146)
   153 '(153 149 148 145)
   154 '(154 153 149 145)
   155 '(155 151 150 148)
   156 '(156 153 151 147)
   157 '(157 155 152 151)
   158 '(158 153 152 150)
   159 '(159 156 153 148)
   160 '(160 158 157 155)
   161 '(161 159 158 155)
   162 '(162 158 155 154)
   163 '(163 160 157 156)
   164 '(164 159 158 152)
   165 '(165 162 157 156)
   166 '(166 164 163 156)
   167 '(167 165 163 161)
   168 '(168 162 159 152)
   169 '(169 164 163 161)
   170 '(170 169 166 161)
   171 '(171 169 166 165)
   172 '(172 169 165 161)
   173 '(173 171 168 165)
   174 '(174 169 166 165)
   175 '(175 173 171 169)
   176 '(176 167 165 164)
   177 '(177 175 174 172)
   178 '(178 176 171 170)
   179 '(179 178 177 175)
   180 '(180 173 170 168)
   181 '(181 180 175 174)
   182 '(182 181 176 174)
   183 '(183 179 176 175)
   184 '(184 177 176 175)
   185 '(185 184 182 177)
   186 '(186 180 178 177)
   187 '(187 182 181 180)
   188 '(188 186 183 182)
   189 '(189 187 184 183)
   190 '(190 188 184 177)
   191 '(191 187 185 184)
   192 '(192 190 178 177)
   193 '(193 189 186 184)
   194 '(194 192 191 190)
   195 '(195 193 192 187)
   196 '(196 194 187 185)
   197 '(197 195 193 188)
   198 '(198 193 190 183)
   199 '(199 198 195 190)
   200 '(200 198 197 195)
   201 '(201 199 198 195)
   202 '(202 198 196 195)
   203 '(203 202 196 195)
   204 '(204 201 200 194)
   205 '(205 203 200 196)
   206 '(206 201 197 196)
   207 '(207 206 201 198)
   208 '(208 207 205 199)
   209 '(209 207 206 204)
   210 '(210 207 206 198)
   211 '(211 203 201 200)
   212 '(212 209 208 205)
   213 '(213 211 208 207)
   214 '(214 213 211 209)
   215 '(215 212 210 209)
   216 '(216 215 213 209)
   217 '(217 213 212 211)
   218 '(218 217 211 210)
   219 '(219 218 215 211)
   220 '(220 211 210 208)
   221 '(221 219 215 213)
   222 '(222 220 217 214)
   223 '(223 221 219 218)
   224 '(224 222 217 212)
   225 '(225 224 220 215)
   226 '(226 223 219 216)
   227 '(227 223 218 217)
   228 '(228 226 217 216)
   229 '(229 228 225 219)
   230 '(230 224 223 222)
   231 '(231 229 227 224)
   232 '(232 228 223 221)
   233 '(233 232 229 224)
   234 '(234 232 225 223)
   235 '(235 234 229 226)
   236 '(236 229 228 226)
   237 '(237 236 233 230)
   238 '(238 237 236 233)
   239 '(239 238 232 227)
   240 '(240 237 235 232)
   241 '(241 237 233 232)
   242 '(242 241 236 231)
   243 '(243 242 238 235)
   244 '(244 243 240 235)
   245 '(245 244 241 239)
   246 '(246 245 244 235)
   247 '(247 245 243 238)
   248 '(248 238 234 233)
   249 '(249 248 245 242)
   250 '(250 247 245 240)
   251 '(251 249 247 244)
   252 '(252 251 247 241)
   253 '(253 252 247 246)
   254 '(254 253 252 247)
   255 '(255 253 252 250)
   256 '(256 254 251 246)
   257 '(257 255 251 250)
   258 '(258 254 252 249)
   259 '(259 257 253 249)
   260 '(260 253 252 250)
   261 '(261 257 255 254)
   262 '(262 258 254 253)
   263 '(263 261 258 252)
   264 '(264 263 255 254)
   265 '(265 263 262 260)
   266 '(266 265 260 259)
   267 '(267 264 261 259)
   268 '(268 267 264 258)
   269 '(269 268 263 262)
   270 '(270 267 263 260)
   271 '(271 265 264 260)
   272 '(272 270 266 263)
   273 '(273 272 271 266)
   274 '(274 272 267 265)
   275 '(275 266 265 264)
   276 '(276 275 273 270)
   277 '(277 274 271 265)
   278 '(278 277 274 273)
   279 '(279 278 275 274)
   280 '(280 278 275 271)
   281 '(281 280 277 272)
   282 '(282 278 277 272)
   283 '(283 278 276 271)
   284 '(284 279 278 276)
   285 '(285 280 278 275)
   286 '(286 285 276 271)
   287 '(287 285 282 281)
   288 '(288 287 278 277)
   289 '(289 286 285 277)
   290 '(290 288 287 285)
   291 '(291 286 280 279)
   292 '(292 291 289 285)
   293 '(293 292 287 282)
   294 '(294 292 291 285)
   295 '(295 293 291 290)
   296 '(296 292 287 285)
   297 '(297 296 293 292)
   298 '(298 294 290 287)
   299 '(299 295 293 288)
   300 '(300 290 288 287)
   301 '(301 299 296 292)
   302 '(302 297 293 290)
   303 '(303 297 291 290)
   304 '(304 303 302 293)
   305 '(305 303 299 298)
   306 '(306 305 303 299)
   307 '(307 305 303 299)
   308 '(308 306 299 293)
   309 '(309 307 302 299)
   310 '(310 309 305 302)
   311 '(311 308 306 304)
   312 '(312 307 302 301)
   313 '(313 312 310 306)
   314 '(314 311 305 300)
   315 '(315 314 306 305)
   316 '(316 309 305 304)
   317 '(317 315 313 310)
   318 '(318 313 312 310)
   319 '(319 318 317 308)
   320 '(320 319 317 316)
   321 '(321 319 316 314)
   322 '(322 321 320 305)
   323 '(323 322 320 313)
   324 '(324 321 320 318)
   325 '(325 323 320 315)
   326 '(326 325 323 316)
   327 '(327 325 322 319)
   328 '(328 323 321 319)
   329 '(329 326 323 321)
   330 '(330 328 323 322)
   331 '(331 329 325 321)
   332 '(332 325 321 320)
   333 '(333 331 329 325)
   334 '(334 333 330 327)
   335 '(335 333 328 325)
   336 '(336 335 332 329)
   337 '(337 336 331 327)
   338 '(338 336 335 332)
   339 '(339 332 329 323)
   340 '(340 337 336 329)
   341 '(341 336 330 327)
   342 '(342 341 340 331)
   343 '(343 338 335 333)
   344 '(344 338 334 333)
   345 '(345 343 341 337)
   346 '(346 344 339 335)
   347 '(347 344 337 336)
   348 '(348 344 341 340)
   349 '(349 347 344 343)
   350 '(350 340 337 336)
   351 '(351 348 345 343)
   352 '(352 346 341 339)
   353 '(353 349 346 344)
   354 '(354 349 341 340)
   355 '(355 354 350 349)
   356 '(356 349 347 346)
   357 '(357 355 347 346)
   358 '(358 351 350 344)
   359 '(359 358 352 350)
   360 '(360 359 335 334)
   361 '(361 360 357 354)
   362 '(362 360 351 344)
   363 '(363 362 356 355)
   364 '(364 363 359 352)
   365 '(365 360 359 356)
   366 '(366 362 359 352)
   367 '(367 365 363 358)
   368 '(368 361 359 351)
   369 '(369 367 359 358)
   370 '(370 368 367 365)
   371 '(371 369 368 363)
   372 '(372 369 365 357)
   373 '(373 371 366 365)
   374 '(374 369 368 366)
   375 '(375 374 368 367)
   376 '(376 371 369 368)
   377 '(377 376 374 369)
   378 '(378 374 365 363)
   379 '(379 375 370 369)
   380 '(380 377 374 366)
   381 '(381 380 379 376)
   382 '(382 379 375 364)
   383 '(383 382 378 374)
   384 '(384 378 369 368)
   385 '(385 383 381 379)
   386 '(386 381 380 376)
   387 '(387 385 379 378)
   388 '(388 387 385 374)
   389 '(389 384 380 379)
   390 '(390 388 380 377)
   391 '(391 390 389 385)
   392 '(392 386 382 379)
   393 '(393 392 391 386)
   394 '(394 392 387 386)
   395 '(395 390 389 384)
   396 '(396 392 390 389)
   397 '(397 392 387 385)
   398 '(398 393 392 384)
   399 '(399 397 390 388)
   400 '(400 398 397 395)
   401 '(401 399 392 389)
   402 '(402 399 398 393)
   403 '(403 398 395 394)
   404 '(404 400 398 397)
   405 '(405 398 397 388)
   406 '(406 402 397 393)
   407 '(407 402 400 398)
   408 '(408 407 403 401)
   409 '(409 406 404 402)
   410 '(410 407 406 400)
   411 '(411 408 401 399)
   412 '(412 409 404 401)
   413 '(413 407 406 403)
   414 '(414 405 401 398)
   415 '(415 413 411 406)
   416 '(416 414 411 407)
   417 '(417 416 414 407)
   418 '(418 417 415 403)
   419 '(419 415 414 404)
   420 '(420 412 410 407)
   421 '(421 419 417 416)
   422 '(422 421 416 412)
   423 '(423 420 418 414)
   424 '(424 422 417 415)
   425 '(425 422 421 418)
   426 '(426 415 414 412)
   427 '(427 422 421 416)
   428 '(428 426 425 417)
   429 '(429 422 421 419)
   430 '(430 419 417 415)
   431 '(431 430 428 426)
   432 '(432 429 428 419)
   433 '(433 430 428 422)
   434 '(434 429 423 422)
   435 '(435 430 426 423)
   436 '(436 432 431 430)
   437 '(437 436 435 431)
   438 '(438 436 432 421)
   439 '(439 437 436 431)
   440 '(440 439 437 436)
   441 '(441 440 433 430)
   442 '(442 440 437 435)
   443 '(443 442 437 433)
   444 '(444 435 432 431)
   445 '(445 441 439 438)
   446 '(446 442 439 431)
   447 '(447 446 441 438)
   448 '(448 444 442 437)
   449 '(449 446 440 438)
   450 '(450 443 438 434)
   451 '(451 450 441 435)
   452 '(452 448 447 446)
   453 '(453 449 447 438)
   454 '(454 449 445 444)
   455 '(455 453 449 444)
   456 '(456 454 445 433)
   457 '(457 454 449 446)
   458 '(458 453 448 445)
   459 '(459 457 454 447)
   460 '(460 459 455 451)
   461 '(461 460 455 454)
   462 '(462 457 451 450)
   463 '(463 456 455 452)
   464 '(464 460 455 441)
   465 '(465 463 462 457)
   466 '(466 460 455 452)
   467 '(467 466 461 456)
   468 '(468 464 459 453)
   469 '(469 467 464 460)
   470 '(470 468 462 461)
   471 '(471 469 468 465)
   472 '(472 470 469 461)
   473 '(473 470 467 465)
   474 '(474 465 463 456)
   475 '(475 471 467 466)
   476 '(476 475 468 466)
   477 '(477 470 462 461)
   478 '(478 477 474 472)
   479 '(479 475 472 470)
   480 '(480 473 467 464)
   481 '(481 480 472 471)
   482 '(482 477 476 473)
   483 '(483 479 477 474)
   484 '(484 483 482 470)
   485 '(485 479 469 468)
   486 '(486 481 478 472)
   487 '(487 485 483 478)
   488 '(488 487 485 484)
   489 '(489 484 483 480)
   490 '(490 485 483 481)
   491 '(491 488 485 480)
   492 '(492 491 485 484)
   493 '(493 490 488 483)
   494 '(494 493 489 481)
   495 '(495 494 486 480)
   496 '(496 494 491 480)
   497 '(497 493 488 486)
   498 '(498 495 489 487)
   499 '(499 494 493 488)
   500 '(500 499 494 490)
   501 '(501 499 497 496)
   502 '(502 498 497 494)
   503 '(503 502 501 500)
   504 '(504 502 490 483)
   505 '(505 500 497 493)
   506 '(506 501 494 491)
   507 '(507 504 501 494)
   508 '(508 505 500 495)
   509 '(509 506 502 501)
   510 '(510 501 500 498)
   511 '(511 509 503 501)
   512 '(512 510 507 504)
   513 '(513 505 503 500)
   514 '(514 511 509 507)
   515 '(515 511 508 501)
   516 '(516 514 511 509)
   517 '(517 515 507 505)
   518 '(518 516 515 507)
   519 '(519 517 511 507)
   520 '(520 509 507 503)
   521 '(521 519 514 512)
   522 '(522 518 509 507)
   523 '(523 521 517 510)
   524 '(524 523 519 515)
   525 '(525 524 521 519)
   526 '(526 525 521 517)
   527 '(527 526 520 518)
   528 '(528 526 522 517)
   529 '(529 528 525 522)
   530 '(530 527 523 520)
   531 '(531 529 525 519)
   532 '(532 529 528 522)
   533 '(533 531 530 529)
   534 '(534 533 529 527)
   535 '(535 533 529 527)
   536 '(536 533 531 529)
   537 '(537 536 535 527)
   538 '(538 537 536 533)
   539 '(539 535 534 529)
   540 '(540 537 534 529)
   541 '(541 537 531 528)
   542 '(542 540 539 533)
   543 '(543 538 536 532)
   544 '(544 538 535 531)
   545 '(545 539 537 532)
   546 '(546 545 544 538)
   547 '(547 543 540 534)
   548 '(548 545 543 538)
   549 '(549 546 545 533)
   550 '(550 546 533 529)
   551 '(551 550 547 542)
   552 '(552 550 547 532)
   553 '(553 550 549 542)
   554 '(554 551 546 543)
   555 '(555 551 546 545)
   556 '(556 549 546 540)
   557 '(557 552 551 550)
   558 '(558 553 549 544)
   559 '(559 557 552 550)
   560 '(560 554 551 549)
   561 '(561 558 552 550)
   562 '(562 560 558 551)
   563 '(563 561 554 549)
   564 '(564 563 561 558)
   565 '(565 564 559 554)
   566 '(566 564 561 560)
   567 '(567 563 557 556)
   568 '(568 558 557 551)
   569 '(569 568 559 557)
   570 '(570 563 558 552)
   571 '(571 569 566 561)
   572 '(572 571 564 560)
   573 '(573 569 567 563)
   574 '(574 569 565 560)
   575 '(575 572 570 569)
   576 '(576 573 572 563)
   577 '(577 575 574 569)
   578 '(578 562 556 555)
   579 '(579 572 570 567)
   580 '(580 579 576 574)
   581 '(581 575 574 568)
   582 '(582 579 576 571)
   583 '(583 581 577 575)
   584 '(584 581 571 570)
   585 '(585 583 582 577)
   586 '(586 584 581 579)
   587 '(587 586 581 576)
   588 '(588 577 572 571)
   589 '(589 586 585 579)
   590 '(590 588 587 578)
   591 '(591 587 585 582)
   592 '(592 591 573 568)
   593 '(593 588 585 584)
   594 '(594 586 584 583)
   595 '(595 594 593 586)
   596 '(596 592 591 590)
   597 '(597 588 585 583)
   598 '(598 597 592 591)
   599 '(599 593 591 590)
   600 '(600 599 590 589)
   601 '(601 600 597 589)
   602 '(602 596 594 591)
   603 '(603 600 599 597)
   604 '(604 600 598 589)
   605 '(605 600 598 595)
   606 '(606 602 599 591)
   607 '(607 600 598 595)
   608 '(608 606 602 585)
   609 '(609 601 600 597)
   610 '(610 602 600 599)
   611 '(611 609 607 601)
   612 '(612 607 602 598)
   613 '(613 609 603 594)
   614 '(614 613 612 607)
   615 '(615 614 609 608)
   616 '(616 614 602 597)
   617 '(617 612 608 607)
   618 '(618 615 604 598)
   619 '(619 614 611 610)
   620 '(620 619 618 611)
   621 '(621 616 615 609)
   622 '(622 612 610 605)
   623 '(623 614 613 612)
   624 '(624 617 615 612)
   625 '(625 620 617 613)
   626 '(626 623 621 613)
   627 '(627 622 617 613)
   628 '(628 626 617 616)
   629 '(629 627 624 623)
   630 '(630 628 626 623)
   631 '(631 625 623 617)
   632 '(632 629 619 613)
   633 '(633 632 631 626)
   634 '(634 631 629 627)
   635 '(635 631 625 621)
   636 '(636 632 628 623)
   637 '(637 636 628 623)
   638 '(638 637 633 632)
   639 '(639 636 635 629)
   640 '(640 638 637 626)
   641 '(641 640 636 622)
   642 '(642 636 633 632)
   643 '(643 641 640 632)
   644 '(644 634 633 632)
   645 '(645 641 637 634)
   646 '(646 635 634 633)
   647 '(647 646 643 642)
   648 '(648 647 626 625)
   649 '(649 648 644 638)
   650 '(650 644 635 632)
   651 '(651 646 638 637)
   652 '(652 647 643 641)
   653 '(653 646 645 643)
   654 '(654 649 643 640)
   655 '(655 653 639 638)
   656 '(656 646 638 637)
   657 '(657 656 650 649)
   658 '(658 651 648 646)
   659 '(659 657 655 644)
   660 '(660 657 656 648)
   661 '(661 657 650 649)
   662 '(662 659 656 650)
   663 '(663 655 652 649)
   664 '(664 662 660 649)
   665 '(665 661 659 654)
   666 '(666 664 659 656)
   667 '(667 664 660 649)
   668 '(668 658 656 651)
   669 '(669 667 665 664)
   670 '(670 669 665 664)
   671 '(671 669 665 662)
   672 '(672 667 666 661)
   673 '(673 666 664 663)
   674 '(674 671 665 660)
   675 '(675 674 672 669)
   676 '(676 675 671 664)
   677 '(677 674 673 669)
   678 '(678 675 673 663)
   679 '(679 676 667 661)
   680 '(680 679 650 645)
   681 '(681 678 672 670)
   682 '(682 681 679 675)
   683 '(683 682 677 672)
   684 '(684 681 671 666)
   685 '(685 684 682 681)
   686 '(686 684 674 673)
   687 '(687 682 675 673)
   688 '(688 682 674 669)
   689 '(689 686 683 681)
   690 '(690 687 683 680)
   691 '(691 689 685 678)
   692 '(692 687 686 678)
   693 '(693 691 685 678)
   694 '(694 691 681 677)
   695 '(695 694 691 686)
   696 '(696 694 686 673)
   697 '(697 689 685 681)
   698 '(698 690 689 688)
   699 '(699 698 689 684)
   700 '(700 698 695 694)
   701 '(701 699 697 685)
   702 '(702 701 699 695)
   703 '(703 702 696 691)
   704 '(704 701 699 692)
   705 '(705 704 698 697)
   706 '(706 697 695 692)
   707 '(707 702 699 692)
   708 '(708 706 704 703)
   709 '(709 708 706 705)
   710 '(710 709 696 695)
   711 '(711 704 703 700)
   712 '(712 709 708 707)
   713 '(713 706 703 696)
   714 '(714 709 707 701)
   715 '(715 714 711 708)
   716 '(716 706 705 704)
   717 '(717 716 710 701)
   718 '(718 717 716 713)
   719 '(719 711 710 707)
   720 '(720 718 712 709)
   721 '(721 720 713 712)
   722 '(722 721 718 707)
   723 '(723 717 710 707)
   724 '(724 719 716 711)
   725 '(725 720 719 716)
   726 '(726 725 722 721)
   727 '(727 721 719 716)
   728 '(728 726 725 724)
   729 '(729 726 724 718)
   730 '(730 726 715 711)
   731 '(731 729 725 723)
   732 '(732 729 728 725)
   733 '(733 731 726 725)
   734 '(734 724 721 720)
   735 '(735 733 728 727)
   736 '(736 730 728 723)
   737 '(737 736 733 732)
   738 '(738 730 729 727)
   739 '(739 731 723 721)
   740 '(740 737 728 716)
   741 '(741 738 733 732)
   742 '(742 741 738 730)
   743 '(743 742 731 730)
   744 '(744 743 733 731)
   745 '(745 740 738 737)
   746 '(746 738 733 728)
   747 '(747 743 741 737)
   748 '(748 744 743 733)
   749 '(749 748 743 742)
   750 '(750 746 741 734)
   751 '(751 750 748 740)
   752 '(752 749 732 731)
   753 '(753 748 745 740)
   754 '(754 742 740 735)
   755 '(755 754 745 743)
   756 '(756 755 747 740)
   757 '(757 756 751 750)
   758 '(758 757 746 741)
   759 '(759 757 756 750)
   760 '(760 757 747 734)
   761 '(761 760 759 758)
   762 '(762 761 755 745)
   763 '(763 754 749 747)
   764 '(764 761 759 758)
   765 '(765 760 755 754)
   766 '(766 757 747 744)
   767 '(767 763 760 759)
   768 '(768 764 751 749)
   769 '(769 763 762 760)
   770 '(770 768 765 756)
   771 '(771 765 756 754)
   772 '(772 767 766 764)
   773 '(773 767 765 763)
   774 '(774 767 760 758)
   775 '(775 771 769 768)
   776 '(776 773 764 759)
   777 '(777 776 767 761)
   778 '(778 775 762 759)
   779 '(779 776 771 769)
   780 '(780 775 772 764)
   781 '(781 779 765 764)
   782 '(782 780 779 773)
   783 '(783 782 776 773)
   784 '(784 778 775 771)
   785 '(785 780 776 775)
   786 '(786 782 780 771)
   1024 '(1024 1015 1002 1001)
   2048 '(2048 2035 2034 2029)
   4096 '(4096 4095 4081 4069)))

(defun taps (n)
  "return a list of register taps (counting from the left) for a polynomial of order n, favoring LFSR-4 taps over LFSR-2."
  (mapcar #'(lambda (x) (- n x))
          (or
           (getf *lfsr4* n)
           (getf *lfsr2* n))))

