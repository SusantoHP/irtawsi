
server<-function(input,output,session){

  output$flowchart<-renderPlot({
    old.par<-par(no.readonly = TRUE)
    par(mar = c(1, 1, 1, 1))
    elpos <- coordinates (c(2, 2,2, 2,2,2))
    for (i in 1:5)
      straightarrow (to = elpos[2*i+1,],from = elpos[2*i-1, ], lwd = 2, arr.pos = 0.6, arr.length = 0.5)
    for (j in 1:6)
      segmentarrow (to = elpos[2*j-1,],from = elpos[2*j, ], lwd = 2, arr.pos = 0.6, arr.length = 0.5)
    pilihbahasa<-ifelse(input$boso=="in_indonesia", "Pilih Bahasa","Choose Language")
    mulai<-ifelse(input$boso=="in_indonesia","Mulai", "Start")
    step1<-ifelse(input$boso=="in_indonesia","Langkah_1 (Langkah Utama)", "Step_1 (Main Step)")
    step1a<-ifelse(input$boso=="in_indonesia","INPUT DATA KAMU", "INPUT YOUR DATA")
    step2<-ifelse(input$boso=="in_indonesia","Langkah 2 (Langkah Utama)", "Step_2 (Main Step)")
    step2a<-ifelse(input$boso=="in_indonesia","KECOCOKAN MODEL", "FINDING FIT MODEL")
    step3<-ifelse(input$boso=="in_indonesia","Langkah_3", "Step_3")
    step3a<-ifelse(input$boso=="in_indonesia","ASUMSI IRT", "IRT ASSUMPTION")
    step4<-ifelse(input$boso=="in_indonesia","Langkah_4", "Step_4")
    step4a<-ifelse(input$boso=="in_indonesia","PARAMETER", "PARAMETER")
    step5<-ifelse(input$boso=="in_indonesia","Langkah_5", "step_5")
    step5a<-ifelse(input$boso=="in_indonesia","ICC, IIC,TIC & Download", "ICC, IIC,TIC & Download")

    textrect (elpos[1,], 0.15, 0.05,lab = pilihbahasa, box.col = "grey",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[2,], 0.1,0.05, lab = mulai, box.col = "green",shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[3,], 0.15, 0.05,lab = step1, box.col = "green",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[4,], 0.15, 0.05, lab = step1a, box.col = "grey",shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[5,], 0.15, 0.05, lab = step2,box.col = "green",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[6,], 0.15, 0.05, lab = step2a,box.col = "grey",shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[7,], 0.15, 0.05, lab = step3,box.col = "green",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[8,], 0.15, 0.05, lab = step3a,box.col = "grey",shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[9,], 0.15, 0.05, lab = step4,box.col = "green",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[10,], 0.15, 0.05, lab = step4a,box.col = "grey",shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[11,], 0.15, 0.05, lab = step5,box.col = "green",shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect(elpos[12,], 0.15, 0.05, lab = step5a,box.col = "grey",shadow.col = "red", shadow.size = 0.005, cex = 1.5)

    par(old.par)
    })



  observeEvent(input$petunjuk, {updateTabItems(session, "inTabset", selected = 'h1')})
  observeEvent(input$inputdataok, {updateTabItems(session, "inTabset", selected = 'h2')})
  observeEvent(input$fitingmodel, {updateTabItems(session, "inTabset", selected = 'h3')})
  observeEvent(input$assumptionprove, {updateTabItems(session, "inTabset", selected = 'h4')})
  observeEvent(input$parameterokk, {updateTabItems(session, "inTabset", selected = 'h5')})
  observeEvent(input$ploticciic, {updateTabItems(session, "inTabset", selected = 'h6')})


  hps1<- reactive({
    ambildataku<- input$ambildata
    if(is.null(ambildataku))      return(NULL)
    if(input$ekstensi=="xlsx"){dataset<- read_xlsx(ambildataku$datapath, sheet=1)}
    if(input$ekstensi=="csv"){dataset<- read.csv(ambildataku$datapath, sep=input$pemisahvariabel)}
    if(input$ekstensi=="txt"){dataset<- read.delim2(ambildataku$datapath, sep=input$pemisahvariabel)}
    i<-1:ncol(dataset)
    colnames(dataset)<-paste0(i)
    p<-as.data.frame(dataset)
    return(p)
  })
  namavariabel <- reactive({
    butir<-colnames(hps1())
    return(butir)
  })
  output$tampildataok1<-renderUI({shinyWidgets:: awesomeCheckboxGroup(inputId = "itemused",label = ifelse(input$boso=="in_indonesia","Hilangkan Centang, untuk butir yang tidak digunakan","unchecked items, for unused Items"),choices = namavariabel(),selected = namavariabel(),inline = TRUE,status = "danger")})

  output$tampildata<-DT::renderDT({DT::datatable(hps(), caption = "", rownames = TRUE,options = list(autoWidth = T, scrollX = TRUE,pageLength = 7,columnDefs = list(list(width = '100px', targets = 1)),paging =T, searching = FALSE), selection='none')})

  hps<-reactive({return( hps1()[,input$itemused])})
  # hps<-reactive({
  #   hps<-hps32()
  # colnames(hps)<-c(1:ncol(hps))
  #   return(hps)
  # })
  kmook1<-reactive({
    kmo<-psych::KMO(hps())
    kmo<-round(kmo[[1]],3)
    return(kmo)})

  kmook<-reactive({
    kmo<-kmook1()
    if(kmo>=0.5){kmsa<-ifelse(input$boso=="in_indonesia",paste("Nilai MSA =",kmo[[1]],". Nilai tersebut lebih besar dari 0.5, sehingga banyak sampel minimal  <b>Mencukupi</b> untuk bisa menggunakan analisis faktor dalam pembuktian Unidimensi."),paste("The MSA Value =",kmo[[1]],". This value is greater than 0.5, so the minimum number of samples is <b>Enough</b> to be able to use factor analysis in unidimensional proof"))}
    if(kmo<0.5 ){kmsa<-ifelse(input$boso=="in_indonesia",paste("Nilai MSA =",kmo[[1]],".Nilai tersebut Kurang dari 0.5, sehingga banyak sampel minimal <b>Tidak Mencukupi</b> untuk bisa menggunakan analisis factor dalam pembuktian Unidimensi"),paste("The MSA Value =",kmo[[1]],". This value is less than  0.5, so the minimum number of samples is <b>Not Enough</b> to be able to use factor analysis in unidimensional proof"))}
    return( kmsa)})

  gambarunidim1<-reactive({
    unid1<-psych:: fa.parallel(hps(), fm = 'minres', fa = 'fa')
    return(unid1)})

  gambarunidim<-reactive({
    kmo<-kmook1()
    if (kmo>=0.5){gambarunidim1()}
    if(kmo<0.5){
      old.par<-par(no.readonly = TRUE)
      par(mar = c(10, 4, 0.5, 2))
      plot(x = 0:1, y = 0:1, ann = FALSE,   bty = "o", type = "n",  xaxt = "n", yaxt = "n")
      text(x = 0.5, y = 0.75,ifelse(input$boso=="in_indonesia", "Maaf, data sample Anda tidak mencukupi","Sorry, your sample data is not enough"),cex = 6,col="red",  font=2, adj=0.5)
      text(x = 0.5, y = 0.25,ifelse(input$boso=="in_indonesia", "Silakan tambahkan data sample anda ","Please add your sample data"),cex = 5,col="blue",  font=2, adj=0.5)
      par(old.par)
      }
    })

  kesimunidim<-reactive({
    kmo<-kmook1()
    unid<-gambarunidim1()
    per<-unid[[1]][1]/sum(round(unid[[1]][1:ncol(hps())]),3)
    if(kmo<0.5){unidim<-ifelse(input$boso=="in_indonesia","Maaf data sample Anda tidak mencukupi untuk bisa menggunakan analisis faktor dalam pembuktian unidimensi","Sorry, your sample data is not sufficient to be able to use factor analysis in unidimensional proofs")}
    if(per>=0.2 && kmo>=0.5){unidim<-ifelse(input$boso=="in_indonesia",paste("Ratio=",round(per,4),"=",round(per,4)*100,"%.","Hasil ini menjelaskan bahwa faktor pertama menyumbang setidaknya 20% dari varians (Retnawati, 2014).  Sehingga, asumsi <b>Unidimensi  Terpenuhi</b> atau butir-butir dalam tes mengukur <b>satu laten trait</b>." ),paste("The Ratio=",round(per,4),"=",round(per,4)*100,"%.","These results explain that the first factor accounts for at least 20% of the variance (Retnawati, 2014). Thus, the <b>Unidimension  assumption is met</b> or the items in the test measure <b> one latent trait</b>."))}
    if(per < 0.2 && kmo>=0.5){unidim<-ifelse(input$boso=="in_indonesia",paste("Ratio=",round(per,4),"=",round(per,4)*100,"%.","Hasil ini menjelaskan bahwa faktor pertama <b>tidak</b> menyumbang setidaknya 20% dari varians (Retnawati, 2014).  Sehingga, asumsi <b>unidimensi tidak terpenuhi</b> atau butir-butir dalam tes mengukur <b>lebih dari satu laten trait</b>. <b>Solusi: </b> untuk mengatasi kasus ini, anda bisa menggunakan <b>Model IRT Multidimensi</b>."),paste("The Ratio=",round(per,4),"=",round(per,4)*100,"%.","These results explain that the first factor does <b>not</b> account for at least 20% of the variance (Retnawati, 2014). Thus, the <b> Unidimension assumption is Not met</b>  or the items in the test measure <b>More Than One latent trait</b>. <b>Solution: </b> To overcome this case, you can use the Multidimensional IRT model."))}
    return(unidim)  })

  observeEvent(input$buttoninfor1, {
    shinyWidgets::show_alert(title =  ifelse(input$boso=="in_indonesia","Analisis IRT","IRT Analysis"),btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),       text = tags$span(
      tags$h3(ifelse(input$boso=="in_indonesia","Langkah 1: Unggah data dan Deteksi Awal ","Step 1: Input data and early detection"),style="font-family: 'cursive';color: red; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","a. Unggah data kamu","a. Input your data"),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","b. Pilih format model IRT yang anda gunakan","b. Select IRT model "),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","c. Tekan tombol analisis dan tunggu  proses selama 5-30 detik (Tergantung spek perangkat anda)","c. Press the ANALYSIS button and wait the process for 5-30second (Depends on your device's spec)"),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","d. Lihat hasil deteksi awal","d. See early detection results"),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","e. Hilangkan centang untuk model yang berwarna MERAH pada 'DETEKSI AWAL' atau yang tidak anda gunakan.","e. Uncheck the model that is RED on 'EARLY DETECTION' or models you don't use"),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","f. Jika e sudah ditentukan, Lanjut ke langah2: FINDING FIT MODEL. tunggu  proses selama 5-30 detik (Tergantung spek perangkat anda)" ,"f. If (e) has been determined, Proceed to step 2: FINDING FIT MODEL. wait the process for 5-30second (Depends on your device's spec)"),style="font-family: 'cursive';color: blue; text-align:left "),
      tags$br(),
      tags$h4(ifelse(input$boso=="in_indonesia","Catatan: Perhatikan warna model hasil DETEKSI AWAL. " ,"Note: See the color of the EARLY DETECTION model."),style="font-family: 'cursive';color: red; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","Hijau: Menunjukkan model memiliki solusi Local Maximum. Sehingga, analisis bisa dilanjutkan" ,"Green: Indicates model is a potential Local Maximum solution. So, the analysis can be continued"),style="font-family: 'cursive';color: red; text-align:left "),
      tags$h4(ifelse(input$boso=="in_indonesia","Merah: Menunjukkan model tidak memiliki solusi Local Maximum. Sehingga, analisis tidak bisa dilanjutkan" ,"Red: Indicates the model does not have a Local Maximum solution. So, the analysis cannot be continued"),style="font-family: 'cursive';color: red; text-align:left ")

    ),html = TRUE,width = "50%") })

  observeEvent(input$buttoninfor1a, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h3(ifelse(input$boso=="in_indonesia","Statistika kecocokan model","Model fit statistic"),style="font-family: 'cursive';color: red; text-align:center "),
                               tags$h4(ifelse(input$boso=="in_indonesia","A.statistik berbasis Likelihood ","A.likelihood-based statistics"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","  Membandingan model menggunakan tes rasio Likelihood. Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), Sample-Size Adjusted BIC (SABIC), and Hannan-Quinn (HQ) Criterion.(Phil Chalmers et.al.2022)","Compare nested models using likelihood ratio test (X2), Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), Sample-Size Adjusted BIC (SABIC), and Hannan-Quinn (HQ) Criterion.(Phil Chalmers et.al.2022)"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","B.Statistika fit model M2 (Maydeu-Olivares & Joe, 2006;Cai and Hansen, 2013) ","B. M2 Statistic fit model (Maydeu-Olivares & Joe, 2006;Cai and Hansen, 2013) "),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","C.Statistika fit model C2 (Cai and Monro, 2014).","C. C2 Statistic fit model(Cai and Monro, 2014)."),style="font-family: 'cursive';color: blue; text-align:left ")
                             ),html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor2, {
    shinyWidgets::show_alert( title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                              text = tags$span(
                                tags$h4(ifelse(input$boso=="in_indonesia","Menurut Maydeu-Olivares(2014) Model fit terpenuhi jika memenuhi kondisi dibawah ini","According to Maydeu-Olivares (2014), The model is said to be fit if it satisfies the following conditions"),style="font-family: 'cursive';color: red; text-align:left "),
                                tags$h4("a. p_value >= 0.01. " ,style="font-family: 'cursive';color: blue; text-align:left "),
                                tags$h4("b. RMSEA <= 0.06",style="font-family: 'cursive';color: blue; text-align:left "),
                                tags$h4(ifelse(input$boso=="in_indonesia","Model terbaik pada package ini menggunakan kriteria yang memenuhi (a) dan (b), serta memiliki nili RMSEA terkecil(Maydeu-Olivares, 2014;Paek & Cole, 2019)","The best model in this package uses the criteria that meet (a) and (b) and has the smallest RMSEA value (Maydeu-Olivares, 2014;Paek & Cole, 2019)"),style="font-family: 'cursive';color: blue; text-align:left "),tags$br(),
                                tags$h4(ifelse(input$boso=="in_indonesia","Catatan: Untuk menentukan model fit, anda tidak harus menggunakan kondisi (a) dan (b) di atas. Tetapi, Anda bisa menggunakan kombinasi dari P-value, RSMEA, SRMR, CFI, dan TLI (Sesuaikan dengan referensi yang anda gunakan)","Note: To determine model fit, you do not have to use conditions (a) and (b) above. However, you can use a combination of P-value, RSMEA, SRMR, CFI, and TLI (according to the reference you use)") ,style="font-family: 'cursive';color: red; text-align:left ")
                              ),html = TRUE,width = "75%")})

  observeEvent(input$buttoninfor3, {
    shinyWidgets::show_alert( title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                              text = tags$span(
                                tags$h4(ifelse(input$boso=="in_indonesia","Statistik berbasis Likelihood ","Likelihood based Statistics"),style="font-family: 'cursive';color: red; text-align:left "),
                                tags$h4(ifelse(input$boso=="in_indonesia","a. Model yang paling fit dapat ditentukan menggunakan salah satu dari kriteria AIC,BIC,HQ, atau SABIC","a. The most suitable model can be determined using one of the AIC, BIC, HQ, or SABIC criteria"),style="font-family: 'cursive';color: blue; text-align:left "),
                                tags$h4(ifelse(input$boso=="in_indonesia","b. Penentuan model yang paling fit pada package ini menggunakan kriteria AIC. Anda bisa menggunakan kriteria yang lain dengan melihat tabel di atas, untuk lebih jelasnya lihat contoh dibawah","b. To determine the fittest model in this package using the AIC criteria. You can use other measures by looking at the table above. For more details, see the example below"),style="font-family: 'cursive';color: blue; text-align:left "),tags$br(),
                                tags$h4(ifelse(input$boso=="in_indonesia","Contoh.","Example."),style="font-family: 'cursive';color: blue; text-align:left "),
                                tags$h4(ifelse(input$boso=="in_indonesia","Misalkan anda menggunakan kriteria BIC, maka model dengan nilai BIC terkecil adalah model yang terbaik. Cara ini berlaku juga ketika anda menggunakan kriteria lainnya.","Suppose you use BIC criteria;  the model with the smallest BIC value is the best. This method also applies when you use other measures."),style="font-family: 'cursive';color: blue; text-align:left ")
                              ),html = TRUE,width = "60%")})

  observeEvent(input$buttoninfors2, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Langkah 2: Menentukan model fit ","Step 2: Determine model fit"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Pilihlah salah satu dari metode pada kotak 'STATISTIKA FIT MODEL', Untuk menentukan model fit ","a. Choose one of the method in the 'FIT MODEL STATISTICS' box,  To determine the fit model."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Model berwarna hijau pada kotak 'MODEL YANG DIREKOMENDASIKAN', merupakan model fit terbaik","b. The Green Model in the 'RECOMMENDED MODEL' box. Indicate the best fit model"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","c. Pada kotak 'PILIH MODEL'. centang model yang direkomendasikan pada (b).","c. See 'CHOOSE MODEL' box and choose the best model recommended in (b)."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","d. Opsional, Lihat perbandingan butir fit untuk setiap model pada kotak 'BUTIR FIT/NON-FIT PADA SETIAP MODEL'","d. Optionally, See a comparison of the fit items for each model in the 'FIT/NON-FIT ITEMS ON EACH MODEL' box "),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h3(ifelse(input$boso=="in_indonesia","e. Jika (c) telah ditentukan. Lanjutkan ke langkah 3: 'IRT ASSUMPTION'","e. If (c) has been determined. Proceed to step 3: 'IRT ASSUMPTION.'"),style="font-family: 'cursive';color: blue; text-align:left "),tags$br(),
                               tags$h4(ifelse(input$boso=="in_indonesia","Catatan: untuk dapat menggunakan kriteria lain dalam menentukan model fit, bacalah information1, information2, dan information3","Note: to be able to use other criteria in determining model fit, read information1, information2, and information3."),style="font-family: 'cursive';color: red; text-align:left ")),
                             html = TRUE,width = "75%")})

  observeEvent(input$buttoninfor4, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Asumsi unidimensi","Unidimension assumption"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","Pada bagian asumsi unidimensi ini anda dapat menggunakan hasil interpretasi yang disediakan oleh package, atau anda dapat melakukan interpretasi sendiri berdasarkan dimensionalitas secara visual ","In this unidimensional assumption section, you can use the interpretation results provided by the package, or you can make your  interpretation based on visual dimensionality"),style="font-family: 'cursive';color: blue; text-align: left ")),
                             html = TRUE,width = "65%")})

  observeEvent(input$buttoninfor5, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Asumsi independensi lokal","Local independence assumption"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Anda bisa menggunakan metode LD (Chen & Thissen, 1997) atau Q3 (Yen,1984) untuk membuktikan lokal independensi. (pilih pada tombol yang tersedia)","a. You can use the LD (Chen & Thissen, 1997) or Q3 (Yen,1984) methods to prove locale independence. (select on the available button)"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Lihat tabel 'Item cause LD'. Tabel ini merupakan ringkasan dari matriks LD diatas. kolom kedua pada tabel ini merupakan pasangan butir yang meneybabkan terjadinya dependensi lokal. dan kolom ketiga merupakan butir yang disaran untuk di hapus.","b. See the 'Item cause LD' table. This table is a summary of the LD matrix above. The second column is the item pair that causes local dependencies. The third column is the item that is suggested to be deleted."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","c. Lihat hasil interpretasi","c. See interpretation"),style="font-family: 'cursive';color: blue; text-align:left ")),
                             html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor6, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter","Parameter invariance assumptions"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Perhatikan gambar diatas. jika setiap titik mengumpul disepanjang garis lurus maka asumsi ini terpenuhi","a. Look at the picture above. If every point converges along a straight line, then this assumption is met."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Pilih metode estimasi parameter ability","b. Select the ability parameter estimation method."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","c. Pilih metode Gendifficulty (Hanya untuk model Politomus)","c. Select the Gendifficulty method (Only for Polytomous models)."),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","d. Bacalah hasil interpretasi","d. Read the interpretation bresult"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","e. Jika pada (d) asumsi invariansi tidak terpenuhi, maka lanjutkan ke  bagian 'MENENTUKAN BUTIR NON-INVARIAN'. Sebaliknya, jika pada (d) asumsi invariansi terpenuhi, maka Lanjutkan ke langkah 4: ITEMS AND ABILITY","e. If in (d) the invariance assumption is not met,  proceed to the 'DETERMINING NON-INVARIANT ITEMS' section. Conversely, if in (d) the invariance assumption is met, then proceed to step 4: ITEMS AND ABILITY"),style="font-family: 'cursive';color: blue; text-align:left ")),
                             html = TRUE,width = "70%")})

  observeEvent(input$buttoninfor7, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Bagian ini hanya digunakan ketika pada bagian 'INVARIANSI PARAMETER' tidak terpenuhi.","This section is only used when the 'PARAMETERS INVARIANCE' section is not met."),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Tentukan butir-butir non-invariansi (Lihatlah catatan dibawah) ","a. Determine non-invariance Items (See notes below)"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Lanjutkan ke bagian 'ITEM AND ABLITY'","b. go to 'ITEM AND ABLITY ' section"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$br(),
                               tags$h4(ifelse(input$boso=="in_indonesia","Catatan: Butir non-invariansi merupakan butir yang menyebabkan asumsi invariansi tidak terpenuhi. Butir ke-n dikatakan non-invarian, jika dua titik dengan label-n saling berjauhan secara ekstrim. Butir non-invarian dapat terjadi pada parameter Diskriminan, Kesulitan atau Psudo Guessing.","Notes: Non-invariance items are items that cause the assumption of invariance not to be met. The nth item is said to be non-invariant, if the two points with the n-label are extremely far from each other. Non-invariant items can occur in the Discriminant, Difficulty or Psudo Guessing parameters"),style="font-family: 'cursive';color: red; text-align:left ")),
                             html = TRUE,width = "75%")})

  observeEvent(input$buttoninfor8, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Mengkategorisasikan Parameter Butir","Categorizing Item Parameters"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Kategorisasikan parameter butir berdasarkan referensi anda (opsional boleh dikosongkan)","a. Categorize item parameters based on your references (Optional can be left blank)"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Hilangkan centang pada butir yang tidak fit dan butir non invarian","b. Uncheck the items that do not fit and non-invariance items"),style="font-family: 'cursive';color: blue; text-align:left ")),

                             html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor9, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Estimasi kemampuan","Ability estimation"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a. Kemampuan diestimasi menggunakan metode yang anda pilih pada bagian parameter invriansi","a. The ability is estimated using the method you choose in the Parameter Invariance section."),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b. Estimasi kemampuan pada tabel diatas masih menggunakan semua butir. sehingga, jika terdapat butir yang tidak fit. maka informasi pada tabel ini tidak dapat digunakan.  untuk melakukan penskoran dengan menggunakan butir yang fit saja anda bisa gunakan package catR","b. The ability in the table above are estimated using all items.so, if there are items that do not fit. then the information in this table cannot be used. To score using only fit items, you can use the catR package"),style="font-family: 'cursive';color: blue; text-align:left ")),
                             html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor10, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h5(ifelse(input$boso=="in_indonesia","Tekan tombol (previous atau next) untuk melihat seluruh kurva karakteristik butir (ICC)","Press the button (previous or next) to view all Item Characteristic Curve (ICC)"),style="font-family: 'cursive';color: red; text-align:left ")),
                             html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor11, {
    shinyWidgets::show_alert(
      title = "",
      btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
      text = tags$span(
        tags$h4(ifelse(input$boso=="in_indonesia","Tekan tombol (previous atau next) untuk melihat seluruh gambar fungsi Informasi Butir (IIC)","Press the button (previous or next) to view all images of the Item Information function (IIC) "),style="font-family: 'cursive';color: red; text-align:left ")),
      html = TRUE,width = "60%")})

  observeEvent(input$buttoninfor12, {
    shinyWidgets::show_alert(title = "",btn_labels = ifelse(input$boso=="in_indonesia","Kembali","Back"),
                             text = tags$span(
                               tags$h4(ifelse(input$boso=="in_indonesia","Fungsi Informasi Tes ","Test Information Function"),style="font-family: 'cursive';color: red; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","a.Tekan Tombol 'All_Item' atau 'All_FitItem' untuk Mengetahui tentan Informasi Tes ","1. Press All_Item or All_FitItem button to know Information about The Test"),style="font-family: 'cursive';color: blue; text-align:left "),
                               tags$h4(ifelse(input$boso=="in_indonesia","b.Silakan unduh hasil analisis","b. Download the Analysis Result"),style="font-family: 'cursive';color: blue; text-align:left ")),
                             html = TRUE,width = "50%")})

  output$labelboxtentang<-renderText({  ifelse(input$boso=="in_indonesia","TENTANG PACKAGE","ABOUT THE PACKAGE")})
  output$ketdeskripsi<-renderText({ifelse(input$boso=="in_indonesia","Analisis data Dichotomous dan polytomous menggunakan model unidimensional Item Response Theory (Chalmers (2012) <doi:10.18637/jss.v048.i06>) dengan Graphical User Interface yang user friendly.  Cocok jika digunakan oleh pemula yang sedang belajar Item Response Theory.","Analysis of Dichotomous and polytomous data using unidimensional Item Response Theory model (Chalmers (2012) <doi:10.18637/jss.v048.i06>) with user friendly Graphical User Interface. Suitable for  beginners who are learning Item Response Theory." )})
  output$labelboxstepstep<-renderText({ifelse(input$boso=="in_indonesia","LANGKAH-LANGKAH MENGGUNAKAN PACKAGE","THE STEPS TO USE THIS PACKAGE")})
  output$labelboxinput<-renderText({ifelse(input$boso=="in_indonesia","MENGINPUT DATA DAN MEMILIH MODEL","INPUTTING DATA AND CHOOSING MODEL")})
  output$labelboxdata<-renderText({ifelse(input$boso=="in_indonesia","DATA KAMU","YOUR DATA")})
  output$labelinput<-renderText({ifelse(input$boso=="in_indonesia","Unggah data kamu","Upload your data")})
  output$labelekstensi<-renderText({ifelse(input$boso=="in_indonesia","Ekstensi file ","File extention")  })
  output$labelcls<-renderText({    ifelse(input$boso=="in_indonesia","Format Data","Data Format")  })
  output$labelboxmetodefit<-renderText({    ifelse(input$boso=="in_indonesia","STATISIKA FIT MODEL","FIT MODEL  STATISTIC")  })
  output$labelboxrecom<-renderText({    ifelse(input$boso=="in_indonesia","MODEL YANG DIREKOMENDASIKAN ","RECOMMENDED MODEL")  })
  output$labelboxpilihtbk<-renderText({    ifelse(input$boso=="in_indonesia","PILIH MODEL ","CHOOSE MODEL")  })
  output$labelboxsumarymodel<-renderText({    ifelse(input$boso=="in_indonesia"," BUTIR FIT/NON-FIT PADA SETIAP MODEL","FIT/NON-FIT ITEMS ON EACH MODEL")  })
  output$labelboxcompar1<-renderText({    ifelse(input$boso=="in_indonesia","MENENTUKAN FIT MODEL ","DETERMINING MODEL FIT")  })
  output$labelcategoability<-renderText({     ifelse(input$boso=="in_indonesia","KATEGORISASI PARAMETER KEMAMPUAN","CATEGORIZING ABILITY PARAMETER")   })
  output$labelmodelabliti<-renderText({     ifelse(input$boso=="in_indonesia","ESTIMASI SKOR","SCORE ESTIMATION")   })
  output$labelboxcompar<-renderText({    ifelse(input$boso=="in_indonesia","MENENTUKAN MODEL TERBAIK ","CHOOSING THE BEST MODEL")  })

  output$labelboxmsa<-renderText({    ifelse(input$boso=="in_indonesia","KECUKUPAN SAMPEL DAN UJI DIMENSI INSTRUMEN","SAMPLE ADEQUACY AND INSTRUMENT DIMENSION TEST")    })
  output$labelboxunidimvisual<-renderText({    ifelse(input$boso=="in_indonesia","VISUALISASI DIMENSIONALITAS","VISUALIZATION OF DIMENSIONALITY")  })
  output$labeltabsyaunidim<-renderText({    ifelse(input$boso=="in_indonesia","DIMENSIONALITAS"," DIMENTIONALITY")  })
  output$labeltabsyali<-renderText({    ifelse(input$boso=="in_indonesia","UJI INDEPENDENSI LOKAL ","LOCAL INDEPENDENDCY TEST")  })
  output$labelmetodeld<-renderText({    ifelse(input$boso=="in_indonesia","Metode Deteksi LD","LD Detection Method")  })
  output$labelldinterpret<-renderText({    ifelse(input$boso=="in_indonesia","INTERPETASI","INTERPRETATION")  })
  output$labeltabsyainv<-renderText({    ifelse(input$boso=="in_indonesia","INVARIANSI PARAMETER","PARAMETER INVARIANCE")  })
  output$labeltabsyaabipar<-renderText({    ifelse(input$boso=="in_indonesia","PARAMETER KEMAMPUAN","ABILITY PARAMETER")  })
  output$labelinvariankesimpulan<-renderText({    ifelse(input$boso=="in_indonesia","KESIMPULAN INVARIANCE","INVARIANCE CONCLUSION")  })
  output$labelinvariankeputusan<-renderText({    ifelse(input$boso=="in_indonesia","INTERPRETASI GAMBAR","IMAGE INTERPRETATION")  })
  output$labeltabsyainvmore<-renderText({    ifelse(input$boso=="in_indonesia","MENENTUKAN BUTIR NON-INVARIAN","FINDING NON-INVARINACE ITEMS")  })
  output$labelabilityok<-renderText({    ifelse(input$boso=="in_indonesia","Metode Estimasi","Estimation Method")  })
  output$labelboxcatego<-renderText({    ifelse(input$boso=="in_indonesia","KATEGORISASI PARAMETER BUTIR","ITEMS PARAMETER CATEGORIZATION")  })
  output$labelboxbutir<-renderText({    ifelse(input$boso=="in_indonesia","INTERPRETASI PARAMETER BUTIR","ITEMS PARAMETER INTERPRETATION")  })
  output$labeltabitempar<-renderText({    ifelse(input$boso=="in_indonesia","PARAMETER BUTIR","ITEM PARAMETER")  })
  output$deteksiawal<-renderText({    ifelse(input$boso=="in_indonesia","DETEKSI AWAL","EARLY DETECTION")  })
  output$labelketIIC<-renderText({    ifelse(input$boso=="in_indonesia","Kurva Informasi butir dan Standar Error butir pada gambar disamping terlihat saling  berpotongan. Gunakan tabel dibawah ini, untuk membuktikan kedua kurva berpotongan atau tidak","The item information curve and the item standard error in the figure beside appear to intersect each other. Use the following table to prove that both of them intersect or not")  })




  modelrasch<-reactive({  if (input$analisis!=0)      mod1<-mirt(hps(), model = 1, itemtype="Rasch",SE=T,verbose=T)
  return(mod1)  })
  model2pl<-reactive({    if (input$analisis!=0)      mod2<- mirt(hps(), model=1, itemtype="2PL", SE=T, verbose=T)
  return(mod2)  })

  model3pl<-reactive({    if (input$analisis!=0)     mod3<- mirt(hps(), model=1, itemtype="3PL", SE=T, verbose=T)
  return(mod3)  })

  model4pl<-reactive({    if (input$analisis!=0)    mod4<- mirt(hps(), model=1, itemtype="4PL", SE=T, verbose=T)
  return(mod4)  })

  modelgrm<-reactive({if (input$analisis!=0)    mod5<-mirt(hps(), model=1, itemtype="graded", SE=T)
  return(mod5)  })

  modelgpcm<-reactive({    if (input$analisis!=0)     mod6<-mirt(hps(), model=1, itemtype="gpcm", SE=T)
  return(mod6)  })

  modelpcm<-reactive({    if (input$analisis!=0)     mod7<-mirt(hps(), model=paste("F=1-",ncol(hps())," \n START = (1-",ncol(hps()),",a1,1.0) \n FIXED = (1-",ncol(hps()),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm", SE=T)
  return(mod7)  })

  cekeror<-reactive({ tryCatch(expr = { modelrasch()
    print("OK")},error = function(e){print("error")})})
  cekeror2<-reactive({tryCatch(expr = { model2pl()
    print("OK")},error = function(e){print("error")})})
  cekeror3<-reactive({tryCatch(expr = { model3pl()
    print("OK")},error = function(e){print("error")})})
  cekeror4<-reactive({tryCatch(expr = { model4pl()
    print("OK")},error = function(e){print("error")})})
  cekeror5<-reactive({tryCatch(expr = { modelgrm()
    print("OK")},error = function(e){print("error")})})
  cekeror6<-reactive({tryCatch(expr = { modelgpcm()
    print("OK")},error = function(e){print("error")})})
  cekeror7<-reactive({tryCatch(expr = {modelpcm()
    print("OK")},error = function(e){print("error")})})

  checkrasch<-reactive({    ifelse(cekeror()!="error", extract.mirt(modelrasch(), what="secondordertest"),FALSE)  })
  check2pl<-reactive({    ifelse(cekeror2()!="error", extract.mirt(model2pl(), what="secondordertest"),FALSE)  })
  check3pl<-reactive({    ifelse(cekeror3()!="error", extract.mirt(model3pl(), what="secondordertest"),FALSE)  })
  check4pl<-reactive({    ifelse(cekeror4()!="error", extract.mirt(model4pl(), what="secondordertest"),FALSE)  })
  checkgrm<-reactive({    ifelse(cekeror5()!="error", extract.mirt(modelgrm(), what="secondordertest"),FALSE)   })
  checkgpcm<-reactive({    ifelse(cekeror6()!="error", extract.mirt(modelgpcm(), what="secondordertest"),FALSE) })
  checkpcm<-reactive({    ifelse(cekeror7()!="error", extract.mirt(modelpcm(), what="secondordertest"),FALSE)  })

  output$ibox <- renderbs4InfoBox({  bs4InfoBox(title = "Rasch",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && checkrasch()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & checkrasch()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox2 <- renderbs4InfoBox({ bs4InfoBox(title = "2PL",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && check2pl()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & check2pl()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox3 <- renderbs4InfoBox({ bs4InfoBox(title = "3PL",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && check3pl()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & check3pl()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox4 <- renderbs4InfoBox({ bs4InfoBox(title = "4PL",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && check4pl()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & check4pl()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox5 <- renderbs4InfoBox({ bs4InfoBox(title = "GRM",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && checkgrm()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & checkgrm()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox6 <- renderbs4InfoBox({ bs4InfoBox(title = "GPCM",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && checkgpcm()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & checkgpcm()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})
  output$ibox7 <- renderbs4InfoBox({ bs4InfoBox(title = "PCM",color = ifelse(input$analisis==0,"white",ifelse(input$analisis!=0 && checkpcm()==TRUE,"success","danger")),fill = ifelse(input$analisis==0,FALSE,TRUE), gradient = ifelse(input$analisis==0,FALSE,TRUE), icon = icon(ifelse(input$analisis==0,"circle-xmark",ifelse(input$analisis!=0 & checkpcm()==TRUE, "circle-check","circle-xmark")),verify_fa = FALSE))})

  jenismodel<-reactive({ifelse (input$pilihmodel=="DHICOTOMOUS",butir<-c("Rasch","2PL","3PL","4PL"),butir<-c("GRM","GPCM","PCM"))
    return(butir)  })

  output$selesksiawal<-renderUI({shinyWidgets:: awesomeCheckboxGroup(inputId = "itemusedawal",label = ifelse(input$boso=="in_indonesia","Hilangkan Centang, Jika model tidak digunakan","unchecked model, for unused model"),choices = jenismodel(), selected = jenismodel(), inline = TRUE,  status = c("primary"))})

  output$modelpenentu<-renderUI({shinyWidgets:: radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),inputId = "penentu",label = "",choices =c("NULL", input$itemusedawal),selected = "NULL",direction = "vertical",status = "success",size="xs",individual = F,width = "100%")})

  fitinbahasaya<-reactive({fitinok<-ifelse(input$boso=="in_indonesia","cocok","FIT")
  return(fitinok)  })

  fitinbahasano<-reactive({fitinno<-ifelse(input$boso=="in_indonesia","Tidakcocok","NOTFIT")
  return(fitinno)  })

  metodefitok<-reactive({
    if (input$metodefit=="Likelihood") {mtdfit<-"M2*"}
    if (input$metodefit=="M2*") {mtdfit<-"M2*"}
    if (input$metodefit=="C2") {mtdfit<-"C2"}
    return (mtdfit)  })

  cekmodelfit<-reactive({
    if (input$metodefit=="Likelihood"){
      cekmodelfit<-data.frame(matrix(NA,1,5))
      colnames(cekmodelfit)<-c("AIC","SABIC","HQ","BIC","logLik") }
    if (input$metodefit=="M2*" || input$metodefit=="C2"){
      cekmodelfit<-data.frame(matrix(0,1,9))
      colnames(cekmodelfit)<-c("M2","df","p","RMSEA","RMSEA_5","RMSEA_95","SRMSR","TLI","CFI")}
    return(cekmodelfit)    })

  fitmdlrasch<-reactive({
    if(checkrasch()==FALSE){fitmdl1<-cekmodelfit()}
    if(checkrasch()!=FALSE){
      if (input$metodefit=="Likelihood"){      fitmdl1<-data.frame(anova(modelrasch()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){  fitmdl1<-data.frame(M2(modelrasch(),type =input$metodefit))}}
    return(fitmdl1)})

  fitmdl2pl<-reactive({
    if(check2pl()==FALSE){fitmdl2<-cekmodelfit()}
    if(check2pl()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl2<-data.frame(anova(model2pl()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl2<-data.frame(M2(model2pl(),type =input$metodefit))}}
    return(fitmdl2)    })

  fitmdl3pl<-reactive({
    if(check3pl()==FALSE){fitmdl3<-cekmodelfit()}
    if(check3pl()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl3<-data.frame(anova(model3pl()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl3<-data.frame(M2(model3pl(),type =input$metodefit))}}
    return(fitmdl3)})

  fitmdl4pl<-reactive({
    if(check4pl()==FALSE){fitmdl4<-cekmodelfit()}
    if(check4pl()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl4<-data.frame(anova(model4pl()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl4<-data.frame(M2(model4pl(),type =input$metodefit))}}
    return(fitmdl4)})

  fitmdlgrm<-reactive({
    if(checkgrm()==FALSE){fitmdl5<-cekmodelfit()}
    if(checkgrm()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl5<-data.frame(anova(modelgrm()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl5<-data.frame(M2(modelgrm(),type =input$metodefit))}}
    return(fitmdl5)})

  fitmdlgpcm<-reactive({
    if(checkgpcm()==FALSE){fitmdl6<-cekmodelfit()}
    if(checkgpcm()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl6<-data.frame(anova(modelgpcm()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl6<-data.frame(M2(modelgpcm(),type =input$metodefit))}}
    return(fitmdl6)})

  fitmdlpcm<-reactive({if(checkpcm()==FALSE){fitmdl7<-cekmodelfit()}
    if(checkpcm()!=FALSE){
      if (input$metodefit=="Likelihood"){fitmdl7<-data.frame(anova(modelpcm()))}
      if (input$metodefit=="M2*" || input$metodefit=="C2"){fitmdl7<-data.frame(M2(modelpcm(),type =input$metodefit))}}
    return(fitmdl7)})

  cekitemfit<-reactive({
    cekitemfit<-data.frame(matrix(0,ncol(hps()),5))
    colnames(cekitemfit)<-c("item","S_X2","df.S_X2","RMSEA.S_X2","p.S_X2")
    return(cekitemfit)})

  fit1<-reactive({
    if(checkrasch()==FALSE){fit1<-cekitemfit()}
    if(checkrasch()!=FALSE){fit1<-itemfit(modelrasch())}
    a2<-c(round(fit1$p.S_X2,3))
    itemfit1<-ifelse(a2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit12<-data.frame(a2,itemfit1)
    return(itemfit12)})

  fit2<-reactive({
    if(check2pl()==FALSE){fit2<-cekitemfit()}
    if(check2pl()!=FALSE){fit2<-itemfit(model2pl())}
    b2<-c(round(fit2$p.S_X2,3))
    itemfit2<-ifelse(b2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit22<-data.frame(b2,itemfit2)
    return(itemfit22)})

  fit3<-reactive({
    if(check3pl()==FALSE){fit3<-cekitemfit()}
    if(check3pl()!=FALSE){fit3<-itemfit(model3pl())}
    c2<-c(round(fit3$p.S_X2,3))
    itemfit3<-ifelse(c2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit32<-data.frame(c2,itemfit3)
    return(itemfit32)})

  fit4<-reactive({
    if(check4pl()==FALSE){fit4<-cekitemfit()}
    if(check4pl()!=FALSE){fit4<-itemfit(model4pl())}
    d2<-c(round(fit4$p.S_X2,3))
    itemfit4<-ifelse(d2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit42<-data.frame(d2,itemfit4)
    return(itemfit42)  })

  fit5<-reactive({
    if(checkgrm()==FALSE){fitgrm<-cekitemfit()}
    if(checkgrm()!=FALSE){fitgrm<-itemfit(modelgrm())}
    e2<-c(round(fitgrm$p.S_X2,3))
    itemfit5<-ifelse(e2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit52<-data.frame(e2,itemfit5)
    return(itemfit52) })

  fit6<-reactive({
    if(checkgpcm()==FALSE){fitgpcm<-cekitemfit()  }
    if(checkgpcm()!=FALSE){fitgpcm<-itemfit(modelgpcm())}
    f2<-c(round(fitgpcm$p.S_X2,3))
    itemfit6<-ifelse(f2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit62<-data.frame(f2,itemfit6)
    return(itemfit62)})

  fit7<-reactive({
    if(checkpcm()==FALSE){fitpcm<-cekitemfit()}
    if(checkpcm()!=FALSE){fitpcm<-itemfit(modelpcm())}
    g2<-c(round(fitpcm$p.S_X2,3))
    itemfit7<-ifelse(g2>=0.05,fitinbahasaya(),fitinbahasano())
    itemfit72<-data.frame( g2,itemfit7)
    return(itemfit72)})

  modeldikotomusfit<-reactive({
    kep1<-ifelse(fitmdlrasch()[3]>0.01 && fitmdlrasch()[4]<=0.6 ,"Yes" ,"No")
    kep2<-ifelse(fitmdl2pl()[3]>0.01 && fitmdl2pl()[4]<=0.6 ,"Yes" ,"No")
    kep3<-ifelse(fitmdl3pl()[3]>0.01 && fitmdl3pl()[4]<=0.6  ,"Yes" ,"No")
    kep4<-ifelse(fitmdl4pl()[3]>0.01 && fitmdl4pl()[4]<=0.6  ,"Yes" ,"No")
    Decision<-c(kep1,kep2,kep3,kep4)
    ku<-rbind(fitmdlrasch(),fitmdl2pl(),fitmdl3pl(),fitmdl4pl())
    ku<-cbind(round(ku,2),Decision)
    Model<-c("Rasch","2PL", "3PL","4PL")
    ku<-cbind(Model,ku)
    rownames(ku)<-c("Rasch", "2PL", "3PL","4PL")
    kuok<-ku[input$itemusedawal,]
    return(kuok)  })

  modelpolitomusfit<-reactive({
    kep1<-ifelse(fitmdlgrm()[3]>0.01 && fitmdlgrm()[4]<=0.6 ,"Yes" ,"No")
    kep2<-ifelse(fitmdlgpcm()[3]>0.01 && fitmdlgpcm()[4]<=0.6 ,"Yes" ,"No")
    kep3<-ifelse(fitmdlpcm()[3]>0.01 && fitmdlpcm()[4]<=0.6 ,"Yes" ,"No")
    Decision<-c(kep1,kep2,kep3)
    ku<-rbind(fitmdlgrm(),fitmdlgpcm(),fitmdlpcm())
    ku<-cbind(round(ku,2),Decision)
    Model<-c("GRM", "GPCM","PCM")
    ku<-cbind(Model,ku)
    rownames(ku)<-c("GRM", "GPCM","PCM")
    kuok<-ku[input$itemusedawal,]
    return(kuok)  })

  modeldikotomus<-reactive({
    N_FIT<-c(length(which(fit1()[,1]>=0.05)),length(which(fit2()[,1]>=0.05)),length(which(fit3()[,1]>=0.05)),length(which(fit4()[,1]>=0.05)))
    N_NOT_FIT<-c(ncol(hps())-N_FIT)
    ku<-rbind(fitmdlrasch(),fitmdl2pl(),fitmdl3pl(),fitmdl4pl())
    ku<-cbind(ku,N_FIT,N_NOT_FIT)
    ku<-round(ku,3)
    Model<-c("Rasch","2PL", "3PL","4PL")
    ku<-cbind(Model,ku)
    rownames(ku)<-c("Rasch", "2PL", "3PL","4PL")
    kuok<-ku[input$itemusedawal,]
    return(kuok)  })

  modelpolitomus<-reactive({
    N_FIT<-c(length(which(fit5()[,1]>=0.05)),length(which(fit6()[,1]>=0.05)),length(which(fit7()[,1]>=0.05)))
    N_NOT_FIT<-c(ncol(hps())-N_FIT)
    ku<-rbind(fitmdlgrm(),fitmdlgpcm(),fitmdlpcm())
    ku<-cbind(ku,N_FIT,N_NOT_FIT)
    ku<-round(ku,3)
    Model<-c("GRM", "GPCM","PCM")
    ku<-cbind(Model,ku)
    rownames(ku)<-c("GRM","GPCM","PCM")
    kuok<-ku[input$itemusedawal,]
    return(kuok)  })

  modelterbaikfit<-reactive({
    if (input$pilihmodel=="DHICOTOMOUS"){model1<-modeldikotomusfit()}
    if (input$pilihmodel=="POLYTOMOUS"){model1<-modelpolitomusfit()}
    return(model1)})

  modelterbaik<-reactive({
    if (input$pilihmodel=="DHICOTOMOUS"){model<-modeldikotomus()}
    if (input$pilihmodel=="POLYTOMOUS"){model<-modelpolitomus()}
    return(model)})

  mdlok<-reactive({
    if(input$penentu=="Rasch"){mdlok<-"Rasch"}
    if(input$penentu=="2PL"){mdlok<-"2PL"}
    if(input$penentu=="3PL"){mdlok<-"3PL"}
    if(input$penentu=="4PL"){mdlok<-"4PL"}
    if(input$penentu=="GRM"){mdlok<- "graded"}
    if(input$penentu=="GPCM"){mdlok<-"gpcm"}
    if(input$penentu=="PCM"){mdlok<-"pcm"}
    return(mdlok)})

  mdlokk<-reactive({
    mdlok<-mdlok()
    if (mdlok=="Rasch"){mdlokk<-modelrasch()}
    if (mdlok=="2PL"){mdlokk<-model2pl()}
    if (mdlok=="3PL"){mdlokk<-model3pl()}
    if (mdlok=="4PL"){mdlokk<-model4pl()}
    if (mdlok=="graded"){mdlokk<-modelgrm()}
    if (mdlok=="gpcm"){mdlokk<-modelgpcm()}
    if (mdlok=="pcm"){mdlokk<-modelpcm()}
    return(mdlokk)})

  pngapik<-reactive({
    if (input$metodefit=="Likelihood"){kk<-modelterbaik()
    c1<-ifelse( checkrasch()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="Rasch","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="Rasch","primary","white")))
    c2<-ifelse( check2pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="2PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="2PL","primary","white")))
    c3<-ifelse(check3pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" &&isTRUE( input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="3PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="3PL","primary","white")))
    c4<-ifelse( check4pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="4PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="4PL","primary","white")))
    c5<-ifelse( checkgrm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="GRM","success",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="GRM","primary","white")))
    c6<-ifelse(checkgpcm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="GPCM","success",ifelse(input$pilihmodel=="POLYTOMOUS" &&isTRUE(input$penentu!="NULL") && input$penentu=="GPCM","primary","white")))
    c7<-ifelse( checkpcm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && kk[which.min(kk[,2]),1]=="PCM","success",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu!="NULL") && input$penentu=="PCM","primary","white")))}
    if (input$metodefit=="M2*" || input$metodefit=="C2"){
      kk<-modelterbaikfit()
      kk1<-kk[which(kk[,11]=="Yes"),1:11]
      if(length(which(kk[,11]=="Yes"))==0){}
      c1<-ifelse(checkrasch()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL")&& length(which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="Rasch","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="Rasch","primary","white")))
      c2<-ifelse(check2pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && length(which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="2PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="2PL","primary","white")))
      c3<-ifelse(check3pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && length( which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="3PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" &&isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="3PL","primary","white")))
      c4<-ifelse(check4pl()==FALSE,"danger",ifelse(input$pilihmodel=="DHICOTOMOUS" && isTRUE(input$penentu=="NULL") && length(which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="4PL","success",ifelse(input$pilihmodel=="DHICOTOMOUS" &&isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="4PL","primary","white")))
      c5<-ifelse(checkgrm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && length(which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="GRM","success",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="GRM","primary","white")))
      c6<-ifelse(checkgpcm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && length( which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="GPCM","success",ifelse(isTRUE(input$pilihmodel=="POLYTOMOUS" && input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="GPCM","primary","white")))
      c7<-ifelse(checkpcm()==FALSE,"danger",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu=="NULL") && length(which(kk[,11]=="Yes"))!=0 && kk1[which.min(kk1[,4]),1]=="PCM","success",ifelse(input$pilihmodel=="POLYTOMOUS" && isTRUE(input$penentu!="NULL") && length(which(kk[,11]=="Yes"))!=0 && input$penentu=="PCM","primary","white")))}
    unsurbox<-c(c1,c2,c3,c4,c5,c6,c7)
    return(unsurbox)})

  output$ibox1a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[1]);bs4InfoBox(title = "Rasch" ,color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox2a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[2]);bs4InfoBox(title = "2PL" ,color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox3a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[3]);bs4InfoBox(title = "3PL" ,color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox4a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[4]);bs4InfoBox(title = "4PL" ,color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox5a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[5]);bs4InfoBox(title = "GRM" ,color=color ,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox6a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[6]);bs4InfoBox(title = "GPCM",color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})
  output$ibox7a <- renderbs4InfoBox({color <-ifelse(is.null(input$ambildata),"white",pngapik()[7]);bs4InfoBox(title = "PCM" ,color=color,fill = ifelse(color=="white",FALSE,TRUE), gradient = ifelse(color=="white",FALSE,TRUE), icon = icon(ifelse(color=="white" || color=="danger","circle-xmark", "circle-check"),verify_fa = FALSE))})

  perbandinganitemfit<-reactive({
    if (input$pilihmodel=="DHICOTOMOUS"){pirt1<-data.frame(fit1()[,2],fit2()[,2],fit3()[,2],fit4()[,2])
    colnames(pirt1)<-c("Rasch","2PL","3PL","4PL")
    pirt3<-pirt1[,input$itemusedawal]}
    if (input$pilihmodel=="POLYTOMOUS"){pirt1<-data.frame(fit5()[,2],fit6()[,2],fit7()[,2])
    colnames(pirt1)<-c("GRM","GPCM","PCM")
    pirt3<-pirt1[,input$itemusedawal]}
    return(pirt3)})

  metodepb<-reactive({
    if(input$abilityok=="EAP"){metode="EAP"}
    if(input$abilityok=="EAPSUM"){metode="EAPsum"}
    if(input$abilityok=="MAP"){metode="MAP"}
    if(input$abilityok=="ML"){metode="ML"}
    return(metode)})

  parabutir<-reactive({mdlokk<-mdlokk()
  ability<-round(fscores(mdlokk, method=metodepb(), full.scores=T, full.scores.SE = T),3)
  par.ability<-data.frame("Obserb Ability"=ability[,1],"Erorr Ability"=ability[,2])
  return( par.ability)})

  ko<-reactive({mdlokk<-mdlokk()
  ko<-coef(mdlokk, IRTpars=T, simplify=T)
  ko<-round(ko$items,2)
  ko<-data.frame(ko)
  return(ko)  })

  kodif<-reactive({
    if (input$difficultypoly=="IRF"){tipe="IRF"}
    if (input$difficultypoly=="MEAN"){tipe="mean"}
    if (input$difficultypoly=="MEDIAN"){tipe="median"}
    if (input$difficultypoly=="TRIMMED"){tipe="trimmed"}
    return(tipe)})

  gendif<-reactive({
    mdlokk<-mdlokk()
    difficulties<-data.frame(gen.difficulty(mdlokk, type = kodif()))
    return(difficulties)})

  itemcriteria<-reactive({
    mdlok<-mdlok()
    if (mdlok=="Rasch"){ft1<-fit1()[,2]}
    if (mdlok=="2PL"){ft1<-fit2()[,2]}
    if (mdlok=="3PL"){ft1<-fit3()[,2]}
    if (mdlok=="4PL"){ft1<-fit4()[,2]}
    if (mdlok=="graded"){ft1<-fit5()[,2]}
    if (mdlok=="gpcm"){ft1<-fit6()[,2]}
    if (mdlok=="pcm"){ft1<-fit7()[,2]}
    if(input$boso=="in_indonesia"){criteria<-ifelse( ft1=="cocok","dipakai","dibuang")}
    if(input$boso=="in_English"){criteria<-ifelse(ft1=="FIT" ,"Used","Excluded")}
    return(criteria)})

  leveldifficult<-reactive({
    if(mdlok()=="Rasch"  || mdlok()=="2PL" ||  mdlok()=="3PL" || mdlok()=="4PL"){ ko<-ko()[,2] }
    if(mdlok()=="graded" || mdlok()=="gpcm" ||mdlok()=="pcm" ){   ko<-gendif()   }
    if (is.null( input$hardflagdif) && is.null(input$easyflagdif)){Cdif<- rep("-",times=ncol(hps()))}
    if (!is.null(input$hardflagdif) && !is.null(input$easyflagdif) && input$boso=="in_English"){Cdif<-ifelse(ko>=input$hardflagdif,"Hard",ifelse((ko>=input$easyflagdif & ko<input$hardflagdif),"Medium","Easy"))}
    if (!is.null(input$hardflagdif) && !is.null(input$easyflagdif) && input$boso=="in_indonesia"){Cdif<-ifelse(ko>=input$hardflagdif,"Sulit",ifelse((ko>=input$easyflagdif & ko<input$hardflagdif),"Sedang","Mudah"))}
    return(Cdif)})

  leveldiscriminan<-reactive({
    ko<-ko()
    if (is.null( input$highflagdis) && is.null( input$lowflagdis)){Cdiss<- rep("-",times=ncol(hps()))}
    if (!is.null(input$highflagdis) && !is.null(input$lowflagdis)  && input$boso=="in_English"){Cdiss<-ifelse(ko[,1]>=input$highflagdis,"High",ifelse((ko[,1]>=input$lowflagdis & ko[,1]<input$highflagdis),"Medium","Low"))}
    if (!is.null(input$highflagdis) && !is.null(input$lowflagdis)  && input$boso=="in_indonesia"){Cdiss<-ifelse(ko[,1]>=input$highflagdis,"Tinggi",ifelse((ko[,1]>=input$lowflagdis & ko[,1]<input$highflagdis),"Sedang","Rendah"))}
    return(Cdiss) })

  levelguessing<-reactive({
    ko<-ko()
    if (is.null( input$highflaggues) && is.null( input$lowflaggues)){Cguess<- rep("-",times=ncol(hps()))}
    if (!is.null(input$highflaggues) && !is.null(input$lowflaggues) && input$boso=="in_English"){Cguess<-ifelse(ko[,3]>=input$highflaggues,"High",ifelse((ko[,3]>=input$lowflaggues & ko[,3]<input$highflaggues),"Medium","Low"))}
    if (!is.null(input$highflaggues) && !is.null(input$lowflaggues) && input$boso=="in_indonesia"){Cguess<-ifelse(ko[,3]>=input$highflaggues,"Tinggi",ifelse((ko[,3]>=input$lowflaggues & ko[,3]<input$highflaggues),"Sedang","Rendah"))}
    return(Cguess)})

  levelability<-reactive({
    parabutir<-parabutir()
    if (is.null( input$highflagabil) && is.null( input$lowflagabil)){Cabil<- rep("-",times=nrow(hps()))}
    if (!is.null( input$highflagabil) && !is.null( input$lowflagabil) && input$boso=="in_English"){Cabil<-ifelse(parabutir[,1]>=input$highflagabil,"High",ifelse((parabutir[,1]>=input$lowflagabil & parabutir[,1]<input$highflagabil),"Medium","Low"))}
    if (!is.null( input$highflagabil) && !is.null( input$lowflagabil) && input$boso=="in_indonesia"){Cabil<-ifelse(parabutir[,1]>=input$highflagabil,"Tinggi",ifelse((parabutir[,1]>=input$lowflagabil & parabutir[,1]<input$highflagabil),"Sedang","Rendah"))}
    return(Cabil)})

  characterbutir<-reactive({
    mdlok<-mdlok()
    ko<-ko()
    if (mdlok=="Rasch"){
      charbutir<-data.frame(ko,fit1(),itemcriteria(),leveldifficult())
      colnames(charbutir)<-c("DISCRIMINANT","DIFFICULTY","GUESSING", "INATTENTION","Chi-SQR","ITEM_FIT","ITEM_CRITERIA","DIFFICULTY_Level")
      charbutir<-charbutir[,-4]
      charbutir<-charbutir[,-3]
      charbutir<-charbutir[,-1]}
    if (mdlok=="2PL"){
      charbutir<-data.frame(ko,fit2(), itemcriteria(),leveldiscriminan(),leveldifficult())
      colnames(charbutir)<-c("DISCRIMINANT","DIFFICULTY","GUESSING", "INATTENTION","Chi-SQR","ITEM_FIT","ITEM_CRITERIA","DISCRIMINANT_LEVEL","DIFFICULTY_Level")
      charbutir<-charbutir[,-4]
      charbutir<-charbutir[,-3]}
    if (mdlok=="3PL"){
      charbutir<-data.frame(ko,fit3(), itemcriteria(),leveldiscriminan(),leveldifficult(), levelguessing())
      colnames(charbutir)<-c("DISCRIMINANT","DIFFICULTY","GUESSING", "INATTENTION","Chi-SQR","ITEM_FIT","ITEM_CRITERIA","DISCRIMINANT_LEVEL","DIFFICULTY_Level","GUESSING_LEVEL")
      charbutir<-charbutir[,-4]}
    if (mdlok=="4PL"){
      charbutir<-data.frame(ko,fit4(), itemcriteria(),leveldiscriminan(),leveldifficult(),levelguessing())
      colnames(charbutir)<-c("DISCRIMINANT","DIFFICULTY","GUESSING", "INATTENTION","Chi-SQR","ITEM_FIT","ITEM_CRITERIA","DISCRIMINANT_LEVEL","DIFFICULTY_Level","GUESSING_LEVEL")}
    if (mdlok=="graded" || mdlok=="gpcm" || mdlok=="pcm"){
      charbutir<-data.frame(ko,round( gendif(),3),fit6(),itemcriteria(),leveldiscriminan(),leveldifficult())
      colnames(charbutir)<-c("DISCRIMINANT",paste0("TRESHOLD-",1:(ncol(ko)-1)),"GEN_DIFFICULTY","Chi-SQR","ITEM_FIT","ITEM_CRITERIA","DISCRIMINANT_LEVEL","GEN_DIFFICULTY_Level")}
    return(charbutir)})

  # butirfit<- reactive({
  #     butir<-c(1:nrow(ko()))
  #     return(butir)})
  butirfit<- reactive({
    butir<-colnames(hps())
    return(butir)})
  output$fititemok<-renderUI({shinyWidgets:: awesomeCheckboxGroup(inputId = "itemfitok",label = ifelse(input$boso=="in_indonesia","Hilangkan Centang pada butir-butir yang tidak fit dan non_invarian","Uncheck the items that are not fit and non-invariance"),choices = butirfit(),selected = butirfit(),inline = TRUE,status = c("primary"))})

  newko<-reactive({
    newko<-cbind(ko(),1:nrow(ko()))
    newko<-newko[input$itemfitok,]
    newko1<-newko
    return(newko1[,ncol(newko1)])})

  characterbutirnew<-reactive({
    newcharacterbutir<-characterbutir()[input$itemfitok,]
    return(newcharacterbutir)})

  parabutirok<-reactive({
    peserta<-ifelse(input$boso=="in_indonesia","Peserta","Tes_Taker")
    charabil<-data.frame(parabutir(),levelability())
    charabilok<-data.frame(t( charabil))
    colnames(charabilok)<-paste(peserta,1:nrow(hps()))
    row.names(charabilok)<-c("OBSERV_ABILTY","ERROR_ABILTY","ABILITY_LEVEL")
    return(charabilok)})

  mdlkelompok1<-reactive({
    tipeitem<-mdlok()
    ganjil<- seq_len(nrow(hps())) %% 2
    dataganjil <- hps()[ganjil == 1, ]
    hps11<- dataganjil

    if (tipeitem=="Rasch" || tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL"){mdlkelompok1<-coef(mirt(hps11, model = 1, itemtype=tipeitem,SE=T), IRTpars=T, simplify=T)$items}
    if (tipeitem=="graded" || tipeitem=="gpcm"){
      mdlkelompok11<-coef(mirt(hps11, model = 1, itemtype=tipeitem,SE=T), IRTpars=T, simplify=T)$items
      mdlkelompok12<- gen.difficulty(mirt(hps11, model = 1, itemtype=tipeitem,SE=T), type = kodif())
      mdlkelompok1<-cbind(mdlkelompok11,mdlkelompok12)    }
    if (tipeitem=="pcm"){
      mdlkelompok11<-coef(mirt(hps11, model =paste("F=1-",ncol(hps11)," \n START = (1-",ncol(hps11),",a1,1.0) \n FIXED = (1-",ncol(hps11),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), IRTpars=T, simplify=T)$items
      mdlkelompok12<- gen.difficulty(mirt(hps11, model = paste("F=1-",ncol(hps11)," \n START = (1-",ncol(hps11),",a1,1.0) \n FIXED = (1-",ncol(hps11),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), type = kodif())
      mdlkelompok1<-cbind(mdlkelompok11,mdlkelompok12)    }
    return(mdlkelompok1)
  })

  mdlkelompok2<-reactive({
    tipeitem<-mdlok()
    genap<- seq_len(nrow(hps())) %% 2
    datagenap <- hps()[genap == 0, ]
    hps2<-datagenap
    if (tipeitem=="Rasch" || tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL"){mdlkelompok2<-coef(mirt(hps2, model = 1, itemtype=tipeitem,SE=T), IRTpars=T, simplify=T)$items}
    if (tipeitem=="graded"|| tipeitem=="gpcm"){
      mdlkelompok21<-coef(mirt(hps2, model = 1, itemtype=tipeitem,SE=T), IRTpars=T, simplify=T)$items
      mdlkelompok22<- gen.difficulty(mirt(hps2, model = 1, itemtype=tipeitem,SE=T), type = kodif())
      mdlkelompok2<-cbind(mdlkelompok21,mdlkelompok22)}
    if (tipeitem=="pcm"){
      mdlkelompok21<-coef(mirt(hps2, model =paste("F=1-",ncol(hps2)," \n START = (1-",ncol(hps2),",a1,1.0) \n FIXED = (1-",ncol(hps2),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), IRTpars=T, simplify=T)$items
      mdlkelompok22<- gen.difficulty(mirt(hps2, model = paste("F=1-",ncol(hps2)," \n START = (1-",ncol(hps2),",a1,1.0) \n FIXED = (1-",ncol(hps2),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), type = kodif())
      mdlkelompok2<-cbind(mdlkelompok21,mdlkelompok22)}
    return(mdlkelompok2)})

  invabilit<-reactive({
    abb<-coef(mdlokk(), IRTpars=T, simplify=T)
    hps12k<-rbind("kesulitan"=abb$items[,2], hps())
    hps12k<-data.frame( t(hps12k))
    hpsbaruk<-hps12k[with(hps12k,order(hps12k[,1])),]
    hpsbaruk<-hpsbaruk[,-1]
    hpsbaruk<-t(hpsbaruk)
    return(hpsbaruk)  })

  mdlkelompokk1<-reactive({
    tipeitem<-mdlok()
    n3<-ncol(hps())
    if (n3%%2==0){
      n4<-(n3)/2
      hpsk1<-invabilit()[,1:n4]}
    if (n3%%2==1){
      n4<-(n3+1)/2
      hpsk1<-invabilit()[,1:(n4+1)]}
    if(tipeitem=="Rasch" || tipeitem=="2PL" || tipeitem=="3PL"|| tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm"){invark1<-round(fscores(mirt(hpsk1, model = 1, itemtype=tipeitem,SE=T), method=metodepb(), full.scores=T, full.scores.SE = T),2)}
    if(tipeitem=="pcm"){invark1<-round(fscores(mirt(hpsk1, model = paste("F=1-",ncol(hpsk1)," \n START = (1-",ncol(hpsk1),",a1,1.0) \n FIXED = (1-",ncol(hpsk1),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), method=metodepb(), full.scores=T, full.scores.SE = T),2)}
    return(invark1)  })

  mdlkelompokk2<-reactive({
    tipeitem<-mdlok()
    n3<-ncol(hps())
    if (n3%%2==0){
      n4<-(n3)/2
      hpsk2<-invabilit()[,(n4+1):n3]    }
    if (n3%%2==1){
      n4<-(n3+1)/2
      hpsk2<-invabilit()[,(n4+1):n3]    }
    if(tipeitem=="Rasch" || tipeitem=="2PL" || tipeitem=="3PL"|| tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm"){invark2<-round(fscores(mirt(hpsk2, model = 1, itemtype=tipeitem,SE=T), method=metodepb(), full.scores=T, full.scores.SE = T),2)}
    if(tipeitem=="pcm"){invark2<-round(fscores(mirt(hpsk2, model = paste("F=1-",ncol(hpsk2)," \n START = (1-",ncol(hpsk2),",a1,1.0) \n FIXED = (1-",ncol(hpsk2),",a1) \n FREE =(GROUP,COV_11)"), itemtype="gpcm",SE=T), method=metodepb(), full.scores=T, full.scores.SE = T),2)}
    return(invark2)  })

  invarplot<-reactive({
    invark1<-mdlkelompokk1()
    invark2<-mdlkelompokk2()
    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    tipeitem<-mdlok()
    if (tipeitem=="Rasch" ){pplot=2}
    if(tipeitem=="2PL" || tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm" ){pplot=3}
    if(tipeitem=="3PL"){pplot=4}
    if (tipeitem=="4PL"){pplot=5}
    old.par<-par(no.readonly = TRUE)
    par(mfrow=c(1,pplot))
    if (tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL" ||tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm"){plot(mdlkelompok1[,1],y=mdlkelompok2[,1], xlab = ifelse(input$boso=="in_indonesia","Grup pertama dari parameter Discriminant","The first group of the Discriminant parameters"), ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter Discriminant","The first group of the discriminant parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter Discriminant", "The Invariance of Discriminant Parameters"))
      abline(0,1)}
    if (tipeitem=="Rasch" || tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL"  ){plot(mdlkelompok1[,2],y=mdlkelompok2[,2], xlab = ifelse(input$boso=="in_indonesia","Grup pertama dari parameter kesulitan","The first group of the difficulty parameters") , ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter kesulitan","The second group of the difficulty parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter difficulty", "The Invariance of difficulty Parameters"))
      abline(0,1)}
    if (tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm"){plot(mdlkelompok1[,ncol(mdlkelompok1)],y=mdlkelompok2[,ncol(mdlkelompok2)], xlab =ifelse(input$boso=="in_indonesia","Grup pertama dari parameter difficulty","The first group of the difficulty parameters") , ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter difficulty","The second group of the difficulty parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter difficulty", "The Invariance of difficulty Parameters"))
      abline(0,1)}
    if (tipeitem=="3PL"||tipeitem=="4PL" ){plot(mdlkelompok1[,3],y=mdlkelompok2[,3], xlab = ifelse(input$boso=="in_indonesia","Grup pertama dari parameter guessing","The first group of the guessing parameters") , ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter guessing","The second group of the guessing parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter guessing", "The Invariance of guessing Parameters"))
      abline(0,1)}
    if (tipeitem=="4PL" ){plot(mdlkelompok1[,4],y=mdlkelompok2[,4], xlab = ifelse(input$boso=="in_indonesia","Grup pertama dari parameter inattention","The first group of the inattention parameters") , ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter inattention","The second group of the inattention parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter inattention", "The Invariance of inattention Parameters"))
      abline(0,1)}
    plot(invark1[,1],invark2[,1], xlab =  ifelse(input$boso=="in_indonesia","Grup pertama dari parameter kemampuan","The first group of the ability parameters") , ylab = ifelse(input$boso=="in_indonesia","Grup kedua dari parameter ability","The second group of the ability parameters"), main =ifelse(input$boso=="in_indonesia","Invariansi Parameter Kemampuan", "The Invariance of Ability Parameters"))
    abline(0,1)
    par(old.par)


    })

  invarkondisi1<-reactive({
    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    tipeitem<-mdlok()
    ca<-cor.test(mdlkelompok1[,1],y=mdlkelompok2[,1],method ="pearson")
    a1<-ca$estimate
    a2<-ca$p.value
    if (ca$p.value<=0.05 && a1[[1]]>0){    a3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter daya beda terpenuhi","The  Discriminant parameter invariance assumption  is met")}
    if (ca$p.value<=0.05 && a1[[1]]<=0){    a3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter daya beda tidak terpenuhi","The  Discriminant parameter invariance assumption  is not met")}
    if (ca$p.value>0.05){      a3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter daya beda tidak terpenuhi","The  Discriminant parameter invariance assumption  is not met")}
    vaa<-c(round(a1,3),round(a2,3),a3)
    return(vaa)})

  invarkondisi2<-reactive({
    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    tipeitem<-mdlok()
    cb<-cor.test(mdlkelompok1[,2],y=mdlkelompok2[,2],method ="pearson")
    b1<-cb$estimate
    b2<-cb$p.value
    if (cb$p.value<=0.05 && b1[[1]]>0){ b3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter kesulitan terpenuhi","The  Difficulty invariance assumption  is met")}
    if (cb$p.value<=0.05 && b1[[1]]<=0){  b3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter kesulitan tidak terpenuhi","The  Difficulty invariance assumption is not met")}
    if (cb$p.value>0.05){ b3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter kesulitan tidak terpenuhi","The  Difficulty invariance assumption is not met")}
    vbb<-c(round(b1,3),round(b2,3),b3)
    return(vbb)})

  invarkondisi3<-reactive({
    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    tipeitem<-mdlok()
    cc<-cor.test(mdlkelompok1[,3],y=mdlkelompok2[,3],method ="pearson")
    c1<-cc$estimate
    c2<-cc$p.value
    if (cc$p.value<=0.05 && c1[[1]]>0){c3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter pseudo guessing terpenuhi","The  pseudo guessing invariance assumption is met")}
    if (cc$p.value<=0.05 && c1[[1]]<=0){c3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter pseudo guessing tidak terpenuhi"," The  pseudo guessing invariance assumption is not met")}
    if (cc$p.value>0.05){c3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi parameter pseudo guessing tidak terpenuhi"," The  pseudo guessing invariance assumption is not met")}
    vcc<-c(round(c1,3),round(c2,3),c3)
    return(vcc)})

  invarkondisi4<-reactive({
    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    tipeitem<-mdlok()
    cd<-cor.test(mdlkelompok1[,4],y=mdlkelompok2[,4],method ="pearson")
    d1<-cd$estimate
    d2<-cd$p.value
    if (cd$p.value<=0.05 && d1[[1]]>0){ d3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi Inattention terpenuhi","The  Inattention invariance assumption is met")}
    if (cd$p.value<=0.05 && d1[[1]]<=0){  d3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi Inattention tidak terpenuhi","The  Inattention invariance assumption is  not met")}
    if (cd$p.value>0.05){  d3<-ifelse(input$boso=="in_indonesia","Asumsi invariansi Inattention tidak terpenuhi","The  Inattention invariance assumption is  not met")}
    vdd<-c(round(d1,3),round(d2,3),d3)
    return(vdd)})

  invarkondisi5<-reactive({
    invark1<-mdlkelompokk1()
    invark2<-mdlkelompokk2()
    tipeitem<-mdlok()
    ck1<-cor.test(invark1[,1],y=invark2[,1],method ="pearson")
    u1<-ck1$estimate
    u2<-ck1$p.value
    if (ck1$p.value<=0.05 && u1[[1]]>0){ u3<-ifelse(input$boso=="in_indonesia","Asumsi Invariansi parameter kemampuan terpenuhi","The  Ability invariance assumption is met")}
    if (ck1$p.value<=0.05 && u1[[1]]<=0){u3<-ifelse(input$boso=="in_indonesia","Asumsi Invariansi parameter kemampuan tidak terpenuhi","The  Ability invariance assumption is not met")}
    if (ck1$p.value>0.05){u3<-ifelse(input$boso=="in_indonesia","Asumsi Invariansi parameter kemampuan tidak terpenuhi","The  Ability invariance assumption is not met")}
    vkk<-c(round(u1,3),round(u2,3),u3)
    return(vkk)})

  kesimpulaninvarinsi<-reactive({
    kondisi1<-ifelse(input$boso=="in_indonesia",paste("Berdasarkan hasil tersebut, <b> Parameter butir memenuhi asumsi invariance</b>. Sehingga, parameter butir tidak dipengaruhi oleh kemampuan peserta test atau Parameter butir memiliki karakteristik yang konsisten jika digunakan pada peserta tes yang berbeda dalam populasi.","Selanjutnya, <b>parameter kemampuan juga memenuhi asumsi invariance</b>. Dengan Kata lain, parameter butir tidak mempengaruhi kemampuan peserta tes memberikan respon saat tes. Sehingga dapat disimpulkan bahwa instrument yang anda gunakan memiliki kualitas yang bagus untuk mengukur variabel laten yang hendak di ukur.",sep ="<br/>"),paste("Based on these results, <b>The item parameters invariance assumption is met</b>. Thus, the item parameters are not affected by the ability of the test takers or the item parameters have consistent characteristics when used on different test takers in the population.","Furthermore, <b>the ability invariance assumption is also met </b>. In other words, the item parameters do not affect the test taker's ability to respond during the test. So it can be concluded that the instrument you are using has good quality for measuring the latent variable you want to measure.",sep ="<br/>"))
    kondisi2<-ifelse(input$boso=="in_indonesia",paste("Berdasarkan hasil pada <b> tabel interpretasi</b>, Maka asumsi <b>invariansi tidak terpenuhi </b>.Periksalah parameter butir dan parameter person pada tabel tersebut. Parameter manakah yang menyebabkan asumsi ini tidak terpenuhi.","A. <b> ika parameter butir menjadi penyebab asumsi ini tidak terpenuhi</b>, maka  dapat diatasi dengan menggunakan salah satu solusi berikut.","1) <b>Hapus butir </b> yang menyebabkan invariansi parameter butir tes tidak terpenuhi (<b>butir non-ivariance </b>) (Guenole & Brown, 2014; Xu et al., 2020)." ,"2). <b>Minta saran ahli</b> untuk mememutuskan butir non-invariance harus dihapus atau tetap digunakan (Xu et al., 2020).","Catatan: Butir non-invarian dapat dihapus pada bagian  <b>ITEM AND ABILITY.</b>","B. <b>Jika parameter  ability yang  menjadi penyebab  asumsi ini tidak terpenuhi </b>, maka  dapat diatasi dengan <b>Minta saran ahli</b> untuk tetap menggunakan IRT.",sep ="<br/>"),paste("Based on the results in the interpretation table, the <b>invariance assumption is not met </b>. Check the item parameters and person parameters in this table. Which parameter causes this assumption not to be met.","A. <b>If the item parameter causes this assumption is not met</b>, then it can be overcome by using one of the following solutions.","1) Remove <b> non-ivariance items </b>. (Guenole & Brown, 2014; Xu et al., 2020).","2). <b>Seek expert advice</b> to decide whether non-invariance items should be removed or used (Xu et al., 2020).","Note: non-invarian item can be deleted in the  <b>ITEM AND ABILITY. </b> section.","B. <b>If the ability parameter</b> that causes this assumption is not met then, <b> seek expert advice</b> to continue using IRT.",sep ="<br/>"))

    mdlkelompok1<-mdlkelompok1()
    mdlkelompok2<-mdlkelompok2()
    invark1<-mdlkelompokk1()
    invark2<-mdlkelompokk2()
    tipeitem<-mdlok()
    ck<-cor.test(invark1[,1],y=invark2[,1],method ="pearson")
    ck1<-ck$p.value
    ck2<-ck$estimate[[1]]
    if (tipeitem=="Rasch" ||  tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm"){
      cb<-cor.test(mdlkelompok1[,2],y=mdlkelompok2[,2],method ="pearson")
      cb1<-cb$p.value
      cb2<-cb$estimate[[1]]
      kesimp<-ifelse(cb1<=0.05 && cb2>0 && ck1<=0.05 && ck2>0,kondisi1,kondisi2)      }
    if (tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm" ||tipeitem=="pcm"){
      ca<-cor.test(mdlkelompok1[,1],y=mdlkelompok2[,1],method ="pearson")
      ca1<-ca$p.value
      ca2<-ca$estimate[[1]]
      kesimp<-ifelse(cb1<=0.05 && cb2>0 && ck1<=0.05 && ck2>0 && ca1<=0.05 && ca2>0,kondisi1,kondisi2)      }
    if (tipeitem=="3PL"||tipeitem=="4PL" ){
      cg<-cor.test(mdlkelompok1[,3],y=mdlkelompok2[,3],method ="pearson")
      cg1<-cg$p.value
      cg2<-cg$estimate[[1]]
      kesimp<-ifelse(cb1<=0.05 && cb2>0 && ck1<=0.05 && ck2>0 && ca1<=0.05 && ca2>0 && cg1<=0.05 && cg2>0 ,kondisi1,kondisi2)         }
    if (tipeitem=="4PL" ){
      cu<-cor.test(mdlkelompok1[,4],y=mdlkelompok2[,4],method ="pearson")
      cu1<-cu$p.value
      cu2<-cu$estimate[[1]]
      kesimp<-ifelse(cb1<=0.05 && cb2>0 && ck1<=0.05 && ck2>0 && ca1<=0.05 && ca2>0 && cg1<=0.05 && cg2>0 && cu1<=0.05 && cu2>0,kondisi1,kondisi2)     }
    return(kesimp)})

  invarinterpret<-reactive({
    tipeitem<-mdlok()
    refernsinv<-c("-","-","( Hambleteon, Swaminatan, & Rogers,1991;Retnawati,2014)")
    if (tipeitem=="Rasch" ||  tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm"  ){vb<-invarkondisi2()}
    if (tipeitem=="2PL" ||tipeitem=="3PL"||tipeitem=="4PL" || tipeitem=="graded" || tipeitem=="gpcm" ||tipeitem=="pcm"){va<-invarkondisi1()}
    if (tipeitem=="3PL"||tipeitem=="4PL" ){vc<-invarkondisi3()}
    if (tipeitem=="4PL" ){vd<-invarkondisi4()}

    vk<-invarkondisi5()
    if (tipeitem=="Rasch" ){
      kesminvarpar<-data.frame( rbind(vb,vk,refernsinv))
      rownames(kesminvarpar)<- c(ifelse(input$boso=="in_indonesia","T_Kesulitan","Difficulty"),ifelse(input$boso=="in_indonesia","Kemampuan", "Ability"),ifelse(input$boso=="in_indonesia","Referensi","Reference" ))}
    if (tipeitem=="2PL" || tipeitem=="graded" || tipeitem=="gpcm" || tipeitem=="pcm" ){
      kesminvarpar<-data.frame( rbind(va,vb,vk,refernsinv))
      rownames(kesminvarpar)<- c("Discriminant", "Difficulty ","Ability","Reference")}
    if (tipeitem=="3PL" ){
      kesminvarpar<-data.frame( rbind(va,vb,vc,vk,refernsinv))
      rownames(kesminvarpar)<- c("Discriminant","Difficulty ","P_Guessing","Ability","Reference")}
    if (tipeitem=="4PL" ){
      kesminvarpar<-data.frame( rbind(va,vb,vc,vd,vk,refernsinv))
      rownames(kesminvarpar)<- c("Discriminant","Difficulty ","P_Guessing","Inattention","Ability","Reference")}
    colnames(kesminvarpar)<-c("Correlation", "p_Value","Interpretation")
    return(kesminvarpar)})

  aduhai<-reactive({
    if(input$invparproblem=="ITEMPARAMETER"){adudu<-rbind(mdlkelompok1()[,value3()],mdlkelompok2()[,value3()])}
    if(input$invparproblem=="ABILITYPARAMETER"){adudu<-rbind(mdlkelompokk1()[,1],mdlkelompokk2()[,1])}
    return(adudu)})

  invarinan2<-reactive({
    if(input$pilihmodel=="DHICOTOMOUS"){
      if (value3()==1){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Parameter Diskriminan","Discriminant Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Parameter Diskriminan","Discriminant Parameter Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Diskriminan","Discriminant Data Pair")}
      if (value3()==2){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Parameter Kesulitan","Difficulty Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Parameter Kesulitan","Difficulty Parameter Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Kesulitan","Difficulty Data Pair")}
      if (value3()==3){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Parameter Guessing","Guessing Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Parameter Guessing","Guessing Parameter Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Guessing","Guessing Data Pair")}
      if (value3()==4){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Inattention","The Inattention Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Inattention","The Inattention Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Paramater ke-4","The Inattention Data Pair")}}
    if(input$pilihmodel=="POLYTOMOUS"){
      if (value3()==1){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Parameter Diskriminan","Discriminant Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Parameter Diskriminan","Discriminant Parameter Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Diskriminan","Discriminant Data Pair")}
      if (value3()==2){
        Main<-ifelse(input$boso=="in_indonesia",paste("Invariansi Parameter treshold ke-"),paste("Treshold Parameter Invariant"))
        ylabele<-ifelse(input$boso=="in_indonesia",paste("NIlai Parameter Treshold ke-"),paste("Treshold Parameter value -"))
        xlabele<-ifelse(input$boso=="in_indonesia",paste("Pasangan Data Treshold ke-"),paste("Difficulty Data Pair-"))}
      if (value3()==3){
        Main<-ifelse(input$boso=="in_indonesia",paste("Invariansi Parameter treshold ke-"),paste("Treshold Parameter Invariant"))
        ylabele<-ifelse(input$boso=="in_indonesia",paste("NIlai Parameter Treshold ke-"),paste("Treshold Parameter value -"))
        xlabele<-ifelse(input$boso=="in_indonesia",paste("Pasangan Data Treshold ke-"),paste("Difficulty Data Pair-"))}
      if (value3()==4){
        Main<-ifelse(input$boso=="in_indonesia",paste("Invariansi Parameter treshold ke-"),paste("Treshold Parameter Invariant"))
        ylabele<-ifelse(input$boso=="in_indonesia",paste("NIlai Parameter Treshold ke-"),paste("Treshold Parameter value -"))
        xlabele<-ifelse(input$boso=="in_indonesia",paste("Pasangan Data Treshold ke-"),paste("Difficulty Data Pair-"))}
      if (value3()==5){
        Main<-ifelse(input$boso=="in_indonesia",paste("Invariansi Parameter treshold ke-"),paste("Treshold Parameter Invariant"))
        ylabele<-ifelse(input$boso=="in_indonesia",paste("NIlai Parameter Treshold ke-"),paste("Treshold Parameter value -"))
        xlabele<-ifelse(input$boso=="in_indonesia",paste("Pasangan Data Treshold ke-"),paste("Difficulty Data Pair-"))}
      if (value3()==6){
        Main<-ifelse(input$boso=="in_indonesia","Invariansi Parameter Kesulitan","Difficulty Parameter Invariant")
        ylabele<-ifelse(input$boso=="in_indonesia","Nilai Parameter Kesulitan","Difficulty Parameter Value")
        xlabele<-ifelse(input$boso=="in_indonesia","Pasangan Data Kesulitan","Difficulty Data Pair")}}
    aduh1<-t(aduhai())
    matplot(cbind( aduh1[,1],aduh1[,2]), type = "o",pch=1,col = 3:4,ylab =  ylabele ,xlab = xlabele,xlim = c(1,length(aduh1[,1])),lty = 1,main=Main)
    text(x=aduh1[,1],y=NULL,labels =1:length(aduh1[,1]), col = 1)
    text(x=aduh1[,2],y=NULL,labels = 1:length(aduh1[,1]),col = 2) })

  value3 <- reactiveVal(1)
  observeEvent(input$left2, {newValue3 <- (value3() - 1)
  value3(newValue3)})
  observeEvent(input$right2, {newValue3 <- (value3() + 1)
  value3(newValue3)})

  mdl<-reactive({
    if  (input$metodeld=="LD"){ mdl="LD" }
    if  (input$metodeld=="Q3"){ mdl="Q3"}
    return(mdl) })

  rrr<-reactive({
    mdlokk<-mdlokk()
    rrr<-residuals(mdlokk, type=mdl())
    return(rrr) })


  aaaa<-reactive({
    rrr<-rrr()
    LDX2<-NULL
    for (i in 1:ncol(rrr)){
      for (j in i:nrow(rrr)){
        if (i != j) {LDX2<-rbind(LDX2, cbind(abs(rrr[j,i]),
                                             rrr[j,i], colnames(rrr)[i], rownames(rrr)[j]))}      }}
    data.frame(LDX2[order(LDX2[,1], decreasing=T),][,-1])

    CV<-NULL
    for (i in 1:nrow(rrr)){
      for (j in i:ncol(rrr)){
        if (i != j) {CV<-rbind(CV, cbind(abs(rrr[i,j]), rrr[i,j],
                                         rownames(rrr)[i], colnames(rrr)[j]))}      }}

    aaaa<- data.frame(CV[order(CV[,1], decreasing=T),][,-1])
    xx<-data.frame(round(as.double(aaaa$X1),3),aaaa$X2,aaaa$X3)
    return(xx)  })

  tableldmatrix<-reactive({
    xx<-aaaa()
    if (max(abs(xx[,1]))>0.174 && mdl()=="LD"){
      idl<-xx[which(abs(xx[,1])>0.174),]    }
    if (max(abs(xx[,1]))<=0.174 && mdl()=="LD"){
      idl<-data.frame(max(abs(xx[,1])),0,0)    }
    if (max(abs(xx[,1]))>0.2236 && mdl()=="Q3"){
      idl<-xx[which(abs(xx[,1])>0.2236),]    }
    if (max(abs(xx[,1]))<=0.2236  && mdl()=="Q3"){
      idl<-data.frame(max(abs(xx[,1])),0,0)    }
    rrr<-rrr()
    diag(rrr)<-1
    rrr[lower.tri(rrr)] <- 0
    vvv<-abs(rrr+t(rrr))
    diag(vvv)<-1
    uu<-1:nrow(idl)
    uu2<-c()
    for (i in uu){
      uu1<-length(which(abs(vvv[idl[,2][i],])>abs(vvv[idl[,3][i],])))
      uu2<-c(uu2,uu1)}
    uu3<-ifelse(uu2>ncol(hps())-uu2, idl[,2],idl[,3])
    if (mdl()=="LD"){idl2<-data.frame("Cramer-V"=as.character(idl[,1]),"items pairs"=paste0("(x",idl[,2],",","x",idl[,3],")"),"Excluded" = paste0("x",uu3))}
    if (mdl()=="Q3"){idl2<-data.frame("Q3"=as.character(idl[,1]),"items pairs"=paste0("(x",idl[,2],",","x",idl[,3],")"),"Excluded" = paste0("x",uu3))}
    return(idl2)  })

  LIinterpret<-reactive({
    xx<-aaaa()
    intrpert1a<-paste("Hasil ini mengindikasikan bahwa terjadi <b>kasus dependensi lokal</b> antara dua butir atau lebih, Sehingga Asumsi independensi <b>lokal tidak terpenuhi</b> (Paek & Cole, 2019). Pasangan item yang menyebabkan dependensi lokal dapat dilihat pada tabel <b>Item Cause LD</b>. <b>Kasus ini dapat diatasi</b> dengan memilih salah satu solusi berikut ini:","1) hapus salah satu butir dari setiap pasangan butir yang menyebabkan asumsi ini tidak terpenuhi dan  Lakukan Kaliberasi ulang (Toland, 2014). atau","2)  Anda bisa menggunakan model IRT Non Parameterik (Petersen, 2005). <b>catatan: </b> IRT Non Parametrik tidak disediakan pada Package ini.  atau","3) Abaikan hasil pengujian asumsi independensi lokal ini. Jika dua atau lebih item yang menyebabkan dependensi lokal ini, secara konseptual tidak memiliki keterkaitan konten satu sama lain (Nguyen et al., 2014). atau","4) Abaikan hasil dari Pengujian asumsi Independensi lokal ini. Sesuai pendapat dari (Retnawati, 2014), jika Asumsi Unidimensi terpenuhi, maka secara otomatis asumsi Independesi lokal terpenuhi.",sep ="<br/>")
    intrpert2a<-paste("These results indicate that there is a <b>case of local dependency</b> between two or more items, and <b>the assumption of local independence is not met</b> (Paek & Cole, 2019). Item pairs that cause local dependencies can be seen in the <b></i>Item Cause LD</b></i> table. <b>It case can be resolved</b> by choosing one of the following methods:","1) remove one item from each item pair which causes this assumption not to be met. Then, reanalyze from the first step (Toland, 2014). or","2) You can use the Non-Parametric IRT model (Petersen, 2005). Note: Non-Parametric IRT is not provided in this Package. or","3) Ignore the results of this local independence assumption test. If two or more items cause this local dependency, contextually they are not content related to each other (Nguyen et al., 2014). or","4) Ignore the results of this Local Independence assumption test. In accordance with the opinion of (Retnawati, 2014), if the Unidimensional Assumption is met, then the local independence assumption is automatically met.",sep ="<br/>")
    if (max(abs(xx[,1]))>0.174 && mdl()=="LD"){intrpert<-ifelse(input$boso=="in_indonesia",intrpert1a,intrpert2a)}
    if (max(abs(xx[,1]))<=0.174 && mdl()=="LD"){
      intrpert3a<-paste("Hasil ini menjelaskan bahwa Asumsi <b>Independensi Lokal Terpenuhi</b> (Paek & Cole, 2019). Hasil tersebut dapat dibutkitkan dengan melihat nilai maksimum absolut dari matriks LD  < 0.174 pada tabel <b>Item Cause LD. </b>")
      intrpert4a<- paste("These results explain that the <b>Local Independence assumption is met </b> (Paek & Cole, 2019). These results can be proven by looking at the absolute maximum value of LD-matrix < 0.174 <b>Item Cause LD. </b>")
      intrpert<-ifelse(input$boso=="in_indonesia",intrpert3a,intrpert4a)}
    if (max(abs(xx[,1]))>0.2236 && mdl()=="Q3"){intrpert<-ifelse(input$boso=="in_indonesia",intrpert1a,intrpert2a)}
    if (max(abs(xx[,1]))<=0.2236 && mdl()=="Q3"){
      intrpert3b<-paste("Hasil ini menjelaskan bahwa Asumsi <b>Independensi Lokal TerpenuhI</b> (Paek & Cole, 2019). Hasil tersebut dapat dibutkitkan dengan melihat nilai maksimum absolut dari matriks LD< 0.2236 pada tabel <b>Item Cause LD. </b>")
      intrpert4b<-paste("These results explain that the <b>Local Independence assumption  is met</b> (Paek & Cole, 2019). These results can be proven by looking at the absolute maximum value of LD-matrix < 0.2236 in the <b>Item Cause LD table.</b>")
      intrpert<-ifelse(input$boso=="in_indonesia",intrpert3b,intrpert4b)}
    return(intrpert)})

  value1 <- reactiveVal(1)
  observeEvent(input$left, {newValue1 <- (value1() - 1)
  value1(newValue1)})
  observeEvent(input$right, {newValue1 <- (value1() + 1)
  value1(newValue1)})

  iccplot<-reactive({
    mdlok<-mdlok()
    mdlokk<-mdlokk()
    butirfit<-butirfit()
    li<-paste0(ifelse(input$boso=="in_indonesia", "Kurva Karakteristik Butir ke-","Item Characteristict Curve of Item-"),butirfit[value1()],"-",mdlok)
    print(itemplot(mdlokk,item=value1(),type="trace",theta_lim =c(-4,4),lwd=4,main=li,sub="ability"))})

  iccplotempiris<-reactive({
    mdlokk<-mdlokk()
    print(itemfit(mdlokk, empirical.plot=value1(), empirical.CI=.95,lwd=4))
  })

  printiccplot<-reactive({
    mdlok<-mdlok()
    mdlokk<-mdlokk()
    butirfit<-butirfit()
    for (i in 1:ncol(hps())){
      li<-paste0("ICC For Item-",butirfit[i],"-",mdlok)
      print(itemplot(mdlokk,item=i,type="trace",theta_lim =c(-4,4),lwd=4,main=li,sub="ability"))
      print(itemfit(mdlokk, empirical.plot=i, empirical.CI=.95,lwd=4))}})


  value2 <- reactiveVal(1)
  observeEvent(input$left1, {newValue2 <- (value2() - 1)
  value2(newValue2)})
  observeEvent(input$right1, {newValue2 <- (value2() + 1)
  value2(newValue2)})

  iicitemtrace<-reactive({
    mdlok<-mdlok()
    mdlokk<-mdlokk()
    butirfit<-butirfit()
    li<-paste0(ifelse(input$boso=="in_indonesia", "Kurva Karakteristik dan Informasi dari Butir ke-","Item Characteristict & Information Curve of Item-"),butirfit[value2()],"-",mdlok)
    print(itemplot(mdlokk, item=value2(), type="infotrace",lwd=3,theta_lim = c(-4,4),main=li))})
  iicebiasa<-reactive({
    mdlok<-mdlok()
    mdlokk<-mdlokk()
    butirfit<-butirfit()
    li<-paste0(ifelse(input$boso=="in_indonesia", "Kurva Informasi & Error dari Butir ke-","Information & Error Curve of Item-"),butirfit[value2()],"-",mdlok)
    print(itemplot(mdlokk, item=value2(), type="infoSE",lwd=3,theta_lim = c(-4,4),main=li))})

  pirnticcebiasa<-reactive({
    mdlokk<-mdlokk()
    for (i in 1:ncol(hps())){print(itemplot(mdlokk, item=i, type="infotrace",lwd=3,theta_lim = c(-4,4)))}
    for (i in 1:ncol(hps())){print(itemplot(mdlokk, item=i, type="infoSE",lwd=3,theta_lim = c(-4,4)))}
  })

  nitemfit<-reactive({
    if (input$iictestplh=="All_ITEM"){  lin<-c(1:ncol(hps()))   }
    if (input$iictestplh=="Fit_ITEM"){  lin<-newko()  }
    return(lin)})

  fungsiinformasi<-reactive({
    mdlokk<-mdlokk()
    lin<-nitemfit()
    Theta <-c(-50,50)
    pplot1<-plot(mdlokk, type="info",theta_lim =Theta,which.item=lin,n=10000)
    pplot2<-plot(mdlokk, type="SE",theta_lim =Theta,which.item=lin, n=10000)
    infori<-pplot1$panel.args[[1]]$y
    infore<-pplot2$panel.args[[1]]$y
    inforTetha<-pplot1$panel.args[[1]]$x
    urutan<-which(infori==max(infori))
    xki<-inforTetha[1:(urutan-1)]
    yiki<-infori[1:(urutan-1)] - infore[1:(urutan-1)]
    letakyikimin<-min(which(yiki> 0))
    kunciby<-infori[1:(urutan-1)][letakyikimin]
    kuncib<-xki[letakyikimin]
    xka<-inforTetha[urutan:length(infore)]
    yika<-infori[urutan:length(infore)]-infore[urutan:length(infore)]
    letakyikamax<-max(which(yika> 0))
    kunciay<-infori[urutan:length(infore)][letakyikamax]
    kuncia<-xka[letakyikamax]
    if (kuncib <= -4){kuncib1<--4}
    if (kuncib > -4){kuncib1 <- kuncib}
    if (kuncia >= 4){kuncia1 <- 4}
    if (kuncia < 4){kuncia1 <- kuncia}
    judul<-ifelse(input$boso=="in_indonesia","Daerah Irisan dari fungsi Fungsi Informasi dan Standar Eror  ","The Intersection Area of Function Informationt and Standart Errorr ")
    area <- areainfo(mdlokk, c(kuncib1,kuncia1),which.items = lin)
    Theta1 <- matrix(seq(-4,4,by=0.01))
    info <- testinfo(mdlokk, Theta1)
    plot(info ~ Theta1, type = 'l')
    pick <- Theta1 >= kuncib1 & Theta1 <=kuncia1
    polygon(c(kuncib1, Theta1[pick],kuncia1), c(kunciby, info[pick], kunciay), col='grey', border =  ifelse(input$iictestplh=="Fit_ITEM","blue","red"))
    title(judul)})

  interpretfise<-reactive({
    mdlokk<-mdlokk()
    lin<-nitemfit()
    Theta <-c(-50,50)
    pplot1<-plot(mdlokk, type="info",theta_lim =Theta,which.item=lin,n=10000)
    pplot2<-plot(mdlokk, type="SE",theta_lim =Theta,which.item=lin, n=10000)
    infori<-pplot1$panel.args[[1]]$y
    infore<-pplot2$panel.args[[1]]$y
    inforTetha<-pplot1$panel.args[[1]]$x
    urutan<-which(infori==max(infori))
    xki<-inforTetha[1:(urutan-1)]
    yiki<-infori[1:(urutan-1)] - infore[1:(urutan-1)]
    letakyikimin<-min(which(yiki> 0))
    kunciby<-infori[1:(urutan-1)][letakyikimin]
    kuncib<-xki[letakyikimin]
    xka<-inforTetha[urutan:length(infore)]
    yika<-infori[urutan:length(infore)]-infore[urutan:length(infore)]
    letakyikamax<-max(which(yika> 0))
    kunciay<-infori[urutan:length(infore)][letakyikamax]
    kuncia<-xka[letakyikamax]
    if (kuncib <= -4){kuncib1<--4}
    if (kuncib > -4){kuncib1 <- kuncib}
    if (kuncia >= 4){kuncia1 <- 4}
    if (kuncia < 4){kuncia1 <- kuncia}
    area <- areainfo(mdlokk, c(kuncib1,kuncia1),which.items = lin)
    titikpotkiri<-ifelse(input$boso=="in_indonesia","Perpotongan Fungsi Informasi dan Standar Eror sebelah Kiri ","The Intersection of Information Function and Error Standart on The Left")
    titikpotknan<-ifelse(input$boso=="in_indonesia","Perpotongan Informasi dan Standar Eror sebelah Kanan","The Intersection of Information Function and Error Standart on The Right")
    if (input$iictestplh=="All_ITEM"){
      aabb<-ifelse(input$boso=="in_indonesia","Nilai Total Informasi dari Seluruh Item ","The Total Information Value Of All Item")
      abab<-ifelse(input$boso=="in_indonesia","Nilai Standar Eror dari Seluruh Item","The Error Standart of All Item")
    }
    if (input$iictestplh=="Fit_ITEM"){
      aabb<-ifelse(input$boso=="in_indonesia","Nilai Total Informasi dari Seluruh Item yang FIT","The Total Information Value Of All FIT Item")
      abab<-ifelse(input$boso=="in_indonesia","Nilai Standar Eror dari Seluruh Item yang FIT" ,"The Error Standart of All FIT Item")
    }
    abab1<-ifelse(input$boso=="in_indonesia","Peluang Daerah Arsiran", "The Probability OF shaded Area")
    abab2<-ifelse(input$boso=="in_indonesia","Instrumen Cocok Digunakan untuk Mengukur Kemampuan pada interval","The Instrument is Suitable For Measuring Ability at Interval")
    keterangan<-c(titikpotkiri,titikpotknan,aabb,abab,abab1,abab2)
    nilaifise<-c(paste("(",round(kuncib,3),",",round(kunciby,3),")"),paste("(",round(kuncia,3),",",round(kunciay,3),")"),round(area$TotalInfo, 2),round(1/sqrt(area$TotalInfo),2),paste("(", round(100 * area$Proportion, 2), "%)", sep = ""), paste("(",round(kuncib1,3),",",round(kuncia1,3),")"))
    interpretFISE<-data.frame(keterangan,nilaifise)
    colnames(interpretFISE)<-c(ifelse(input$boso=="in_indonesia","Interpretasi","Interpretation"),ifelse(input$boso=="in_indonesia","Nilai Numerik","Numeric value"))
    return(interpretFISE)  })

  output$perbandinganmodelfit<-renderTable({
    tryCatch(expr = {modelterbaikfit()},error = function(e){NULL})},spacing = "l",width = "100%",bordered = TRUE,align = "c")

  output$perbandinganmodel<-renderTable({tryCatch(expr = {modelterbaik()},error = function(e){NULL})},spacing = "l",width = "100%",bordered = TRUE,align = "c")

  output$perbandinganitemfit<-DT::renderDT({
    tryCatch(expr = {pirt<-t(perbandinganitemfit())
    DT::datatable(pirt, caption = "", rownames =T,options = list(autoWidth = T, scrollX = TRUE,columnDefs = list(list(width = '100px', targets =1)),paging = FALSE, searching = FALSE), selection='none')},error = function(e){NULL})})

  output$lokalindependen<-DT::renderDT({
    tryCatch(expr = {rrr<-rrr()
    DT::datatable(round(rrr,2),class= 'compact stripe', caption = "", rownames = T,options = list(autoWidth = TRUE, scrollX = TRUE,pageLength = 7,columnDefs = list(list(width = '100px', targets = 1)),paging = TRUE, searching = FALSE), selection='none')},error = function(e){NULL})})

  output$kmo<-renderUI({tryCatch(expr = {HTML(kmook())},error = function(e){NULL})})

  output$plotunidimensi<-renderPlot({tryCatch(expr = {gambarunidim()},error = function(e){NULL})})

  output$kesimunidimensi<-renderUI({tryCatch(expr = {HTML(kesimunidim())},error = function(e){NULL})})

  output$itempairs<-renderTable({tryCatch(expr = {tableldmatrix()},error = function(e){NULL})},spacing = "l",bordered = TRUE,width = "100%",align = "c")

  output$ldinterpret<-renderUI({tryCatch(expr = {HTML(LIinterpret())},error = function(e){NULL})})

  output$invariansiplot<-renderPlot({tryCatch(expr = {invarplot()},error = function(e){NULL})})

  output$invariankeputusan<-renderTable({tryCatch(expr = {invarinterpret()},error = function(e){NULL})},rownames = TRUE,bordered = TRUE,spacing = "l",width = "100%",align = "c")

  output$invariankesimpulan<-renderUI({tryCatch(expr = {HTML(kesimpulaninvarinsi())},error = function(e){NULL})})

  output$invarianproblem<-renderPlot({tryCatch(expr = {invarinan2()},error = function(e){NULL})})

  output$modelparam<-DT::renderDT({
    tryCatch(expr = {
      pirt1<-characterbutirnew()
      pirt2<-data.frame(t( pirt1))
      DT::datatable(pirt2, caption = "", rownames = TRUE,options = list(autoWidth = TRUE, scrollX = TRUE,columnDefs = list(list(width = '100px', targets = 1)),paging = FALSE, searching = FALSE), selection='none')},error = function(e){NULL})})

  output$modelabliti<-DT::renderDT({
    tryCatch(expr = {abiliti<-parabutirok()
    DT::datatable(abiliti, caption = "", rownames =T,options = list(autoWidth = T, scrollX = TRUE,columnDefs = list(list(width = '100px', targets = 1)),paging = FALSE, searching = FALSE), selection='none')},error = function(e){NULL})})

  output$iccplot<-renderPlot({tryCatch(expr = {iccplot()},error = function(e){NULL})})

  output$iccplotempiric<-renderPlot({tryCatch(expr = {iccplotempiris()},error = function(e){NULL})})
  output$iicitemtrace<-renderPlot({tryCatch(expr = {iicitemtrace()},error = function(e){NULL})})

  output$iicitem<-renderPlot({tryCatch(expr = {iicebiasa()},error = function(e){NULL})})

  output$iictest<-renderPlot({tryCatch(expr = {fungsiinformasi()},error = function(e){NULL})})

  output$interpretifseok<-renderTable({tryCatch(expr = {interpretfise()},error = function(e){NULL})},bordered = TRUE,spacing = "l",width = "100%")

  language<-reactive({ifelse( input$boso=="in_indonesia","indonesia","english")})

  modelmodel<-reactive({ifelse(input$pilihmodel=="DHICOTOMOUS","DHICOTOMOUS","POLYTOMOUS")})

  output$downloadReport = downloadHandler(filename = "report.html",content = function(file){render("report.Rmd", output_format =html_document(), output_file = file,   quiet = TRUE)})


}

