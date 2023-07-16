package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}

import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}

object LogarithmicIntegral {
  
  def logarithmicIntegral(x: BigComplex, mc: MathContext): BigComplex = {
    if (BigComplex.ZERO.equals(x)) return BigComplex.ZERO
    val theMc = new MathContext(mc.getPrecision << 2, RoundingMode.HALF_EVEN)
    val checkMc = new MathContext(mc.getPrecision << 1, RoundingMode.HALF_UP)
    val log: BigComplex = BigComplexMath.log(x, theMc)
    val llog: BigComplex = if (BigDec.ZERO.compareTo(x.im) == 0 && BigDec.ZERO.compareTo(x.re) <= 0 && BigDec.ONE.compareTo(x.re) >= 0) {
      BigComplexMath.log(log.re(), theMc).re() // li(x) has no imaginary value for real numbers in range 0 < x <= 1
    } else {
      BigComplexMath.log(log, theMc)
    }
    
    val sqrt: BigComplex = MathHelper.complexSqrt(x, theMc)
    
    var sum: BigComplex = BigComplex.ZERO
    var current: BigComplex = BigComplex.ZERO
    var last: BigComplex = null
    var n: Int = 0
    var faculty: BigDec = BigDec.ONE
    var powerTwoNegOne: BigDec = BigDec.ONE
    while (current.round(checkMc) != last) {
      last = current.round(checkMc)
      n += 1
      faculty = faculty.multiply(BigDec.valueOf(n), theMc)
      if (n != 1) powerTwoNegOne = powerTwoNegOne.multiply(BigDec.valueOf(2), theMc)
      
      var subSum = BigComplex.valueOf(0)
      for (k <- 0 to (n - 1) / 2) {
        subSum = subSum.add(BigComplexMath.reciprocal(BigComplex.valueOf((2 * k) + 1), theMc), theMc)
      }
      
      var numer = BigComplexMath.pow(log, BigDec.valueOf(n), theMc)
      if (n % 2 == 0) numer = numer.negate()
      val denom = faculty.multiply(powerTwoNegOne, theMc)
      
      sum = sum.add(numer.divide(denom, theMc).multiply(subSum, theMc), theMc)
      current = sqrt.multiply(sum, theMc)
    }
    llog.add(gamma, theMc).add(current, theMc).round(mc)
  }

  // We have no really fast converging sequence for the euler-mascheroni constant, so we hardcode a lot of digits
  private val gamma: BigDec = new BigDec("""
    0.577215664901532860606512090082402431042159335939923598805767234884867726777664670936947063291746749514631447249807
    08248096050401448654283622417399764492353625350033374293733773767394279259525824709491600873520394816567085323315177
    66115286211995015079847937450857057400299213547861466940296043254215190587755352673313992540129674205137541395491116
    85102807984234877587205038431093997361372553060889331267600172479537836759271351577226102734929139407984301034177717
    78088154957066107501016191663340152278935867965497252036212879226555953669628176388792726801324310104765059637039473
    94957638906572967929601009015125195950922243501409349871228247949747195646976318506676129063811051824197444867836380
    86174945516989279230187739107294578155431600500218284409605377243420328547836701517739439870030237033951832869000155
    81939880427074115422278197165230110735658339673487176504919418123000406546931429992977795693031005030863034185698032
    31083691640025892970890985486825777364288253954925873629596133298574739302373438847070370284412920166417850248733379
    08056275499843459076164316710314671072237002181074504441866475913480366902553245862544222534518138791243457350136129
    77822782881489459098638460062931694718871495875254923664935204732436410972682761608775950880951262084045444779922991
    57248292516251278427659657083214610298214617951957959095922704208989627971255363217948873764210660607065982561990102
    88075612519913751167821764361905705844078357350158005607745793421314498850078641517161519456570617043245075008168705
    23078909370461430668481791649684254915049672431218378387535648949508684541023406016225085155838672349441878804409407
    70106883795111307872023426395226920971608856908382511378712836820491178925944784861991185293910293099059255266917274
    46892044386971114717457157457320393520912231608508682755889010945168118101687497547096936667121020630482716589504932
    73148608749402070067425909182487596213738423114426531350292303175172257221628324883811245895743862398703757662855130
    33143929995401853134141586212788648076110030152119657800681177737635016818389733896639868957932991456388644310370608
    07817448995795832457941896202604984104392250786046036252772602291968299586098833901378717142269178838195298445607916
    05197279736047591025109957791335157917722515025492932463250287476779484215840507599290401855764599018626926776437266
    05711768133655908815548107470000623363725288949554636971433012007913085552639595497823023144039149740494746825947320
    84618524605877669488287953010406349172292185800870677069042792674328444696851497182567809584165449185145753319640633
    11993738215734508749883255608888735280190191550896885546825924544452772817305730108060617701136377318246292466008127
    71621018677446849595142817901451119489342288344825307531187018609761224623176749775564124619838564014841235871772495
    54224820161517657994080629683424289057259473926963863383874380547131967642926837249076087507378528370230468650349051
    20342272174366897928486297290889267897770326246239122618887653005778627436060944436039280977081338369342355085839411
    26709218734414512187803276150509478055466300586845563152454605315113252818891079231491311032344302450933450003076558
    64874222971770033178453915056694015998849291609114002948690208848538169700955156634705544522176403586293982865813123
    87013253588006256866269269977677377306832269009160851045150022610718025546592849389492775958975407615599337826482419
    79506418681437881718508854080367996314239540091964388750078900000627997942809886372992591977765040409922037940427616
    81783715668653066939830916524322705955304176673664011679295901293053744971830800427584863508380804246673509355983232
    41169692148606498927636244329588548737897014897133435384480028904666509028453768962239830488140627305408795911896705
    74938544324786914808533770264067758081275458731117636478787430739206642011251352727499617545053085582356683068322917
    67667704103523153503251012465638615670644984713269596933016786613833333344165790060586749710364689517456959718155376
    40783776501842783459918420159954314490477255523061476701659934163906609120540053221589020913408027822515338528995116
    65452245869185993671220132150144801424230986254604488672569343148870491593044640189164502022405495386291847586293077
    88935064377159660690960468124370230546570316067999258716667524721940977798018636262563358252627942239325486013269353
    07013889374369238428789385127647408565486502815630677404422030644037568263091029175145722344410503693177114521708889
    07446416048688701083862311426128441425960956370400619200579335034155242624026206465693543061258526583452192121497771
    87806958660851633492210483673799459259434037956000219278541837941776020336559467307887983808481631467824149235464914
    88766833684074928938652818630485898203548186243838481759976358490751807914806349439162847054822007549453489861338272
    35730922190030740096800337666844932505567654937530318112516410552492384077645149842395762012781552322944928854557853
    82024891894244185709591955820810007157838403962747998581788088886571683069943606073599042106851142791316969959679230
    08289881560975383380591093603412529986567903895687956734550833629078238626385634907473192752787401665575311901115434
    70018186256971261120126852923129937161403906965112224816615082353643982396620532633322248505191593682690715004315589
    87180278335384544830910724949805788096171799633716703655418004146466753871958694848333154358333064193592948742095147
    88323477484814181497768716944136400566451569361165241615557341419354247213730674683338490544266260383727882175527099
    30958141026136979500786465876771608630804460749802801576962675913897794772214337515470829345879123898433055067223474
    96998494248670672150256927352958506586958899748653556218695804399712516897665416986265386289197754218772193960581700
    11042364141587808103861721015575519237111600498806822916180977324219583289748692271839791904677165426681388933792960
    36815457939611339621922245430151580631743708405608536416031384982969518566952612822123716939368130321296561939718710
    20709800794883391019753510430744182344883333179697827733209114332451430508657345750068739147547077757755991846711830
    85836601594371937184490390617702325365679775967444757475115841957467009973450024544284065850245085856463927912461198
    79093693072019804029303603738838430742162821201635386466226097198958436799430572030149638050832232365825557724534237
    18773743981833330645466290699331112597372195027464689906545715544030391783541975643431573903488386675054274216183105
    00605504642235457084273935493590517627174792994723989086329701019056101077426909264752357403046301592434424649008341
    88630859320685522507790910195858895314328799817570981916829315940453005632543314488517357302698256937253469964013440
    87158010814528786579040866363794507110850510424179769191129261513201031636349808660694862440780066840067169622146371
    81147772683418466463642427340530031380773496119981468617685854631208163164798937964263738356618938313710983289564905
    21148813402974238886863154313297876579912545424333856347200268129048994955042698088213026726358153248067538790323057
    42104033014978878675237786070546886147210099263294251088780197028411792240259109146658480925785719278628214766707408
    78635197142562924278670284077032414375699318832433315590024333047691110092479791180062862022137078006217257329047359
    94398883139279927969397063567628116694054128859081982023838277035483496879734048888293016736770941584654400954862465
    14610135391349685591204023636187215099298065190586168281530287504275452586053319634325957774788134372393949912438061
    43754498590686075185631427255255642593967014980414259818237852576829436395965624388520656548071038845463944537701917
    84571874101186223227802525194362657438242256093567692582387749116073775945140144703190224153559112506138178297421264
    98264161872460631334089192670235979580236584163175567923356621012313358454945905900699842006722602511677438473648243
    85715407146265945642391127170780306371416926386440100571310958960632649637552956769364689410517952000616452021884353
    40473018243930514881984593076296404445687762416528716207276731860632540801428874571198657307471701886603687970364770
    85485287167000362292852883746824660588141175404744606167635430373992375659659369670879231677446856931083821078304831
    59196430021441259702289063203174101149366480952903011716334531917922939242428772837872349569929232136092234947226458
    24375509451533552011761289751733951371782933287158609438662701179184155458726489825139255594379519731786744876992532
    61794233812999412793986026442451960060543681866467098659443659301543762914869795976949965335272100200209679104394825
    47244113342244870054637656840866762153373627461591205470086290571698257353705239761231238412564349411789498615828597
    02097109703919635216812258224756271951938272828520914718237534365525402074620306673047695247009441381456178282666319
    61396735936725726460338849464248947244897848159671530615384671223698712823197847693110571662363732620759597118040151
    43896231706019585709823813924666135279136956376559048616762051297409983149655978602534101294543998672883006244084401
    86181511750687088764671609799292017769624963301575829259618849485872002292248060628812177873383145882551293953651088
    19186512004492315498394773147357868897314204731094538146482263210233107943959746285235412957519106355879230019561813
    12946120301575763401597856817517498423778824473753982474575996867707408545574329424026781938482203540962106060721924
    99082510485400318496319986322156908909761405310716511312932685349852440286448293470591608586995981988903959955918110
    76413446658814525425658815354502888473997532432780902152256952184414529865083551298398022623826497488181158152503459
    97499159664009832014520070480355568589973099815031041922385089745373276129471712681653088670054728865779224670178265
    94827260972290347157414031669731070505078296108260728704291260823119741510147357847810107279711242797602848141151633
    88689690786717577259381524796123789999036661756035882181325467563448309148729626693721988502700188298173017024944214
    06317265197139506221082764507181663910436683295663807307194543211255053620896783230310851714811492858899362870648875
    58438913004718684679858166
  """.replace(" ", "").replace("\n", ""))
}
