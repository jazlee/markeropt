# markeropt
Pemanfaatan perangkat lunak OptiTex saat ini telah banyak membantu mempermudah kegiatan proses produksi pada perusahaan manufaktur garmen secara signifikan khususnya dari sisi perancangan, dan modelling. Perancang dapat dengan mudah merancang elemen-elemen garment dan langsung memvisualisasikannya melalui fasilitas modelling yang tersedia pada aplikasi tersebut. Hasil atas tiap-tiap laying tersebut dapat dioptimasikan pada lembaran-lembaran marker untuk digunakan pada proses pembentukan pola-pola pemotongan kain pada tahapan produksi yang sesungguhnya.

Permasalahan yang ada saat ini adalah proses penyebaran:

- Input Pengulangan dalam Satu Style sehingga Laporannya bisa dijadikan satu. Misalkan input beda fabric, sehingga consumption result beda fabric dapat dijadikan satu laporan.
- Logic Calculation sekarang ini masih berdasarkan Set Priority dan Laying Rule, Bisakah ada pilihan optional berdasarkan 3 atau 5 quantity order tertinggi di setiap perbandingan Quantity.  Misalkan : S = 50, M = 180, L = 175, XL = 90, XXL = 125, Maka  Size yang akan di bagi pertama kali adalah Size  M,L dan XXL. Kemudian sisa quantity di gabung dengan size lain lalu dicari lagi 3 size yang terbesar quantitynya untuk dibagi.
- Panjang Hasil Marker dapat di cross check dengan Panjang Meja yang  ada. Apabila panjang hasil marker melebihi panjang meja akan ada alert atau peringatan mengenai hal tsb.
- Alert untuk Perbandingan antara Consumption Marketing dan Hasil yang  didapatkan dari Program+ Cutting Allowance. ( Biasanya ada allowance untuk Marker sekitar 0.5 - 1"  ke atas dan bawah dari Lebar Marker.
- Perhitungan Harga didapat dari Harga Garment di bandingkan dengan Marketing Offer.
- Tampilan Ratio pada laporan harus lebih simple, size yang tidak ada quantitynya tidak perlu ditampilkan. Jadi di laporan size yang ada rationya saja yang tampil.
- Effisiensi Marker di Laporan.
- Devisiasi mengenai quantity order ( - atau + , kelebihan atau kekurangan potong dilihat dari order yang dikerjakan ) diisi pada pengisian layer tergantung dari allowance buyer.

Aplikasi Marker Optimizer dan Fabric/Material Allocation Planner memiliki beberapa ragam optimasi yang cukup fleksibel untuk membantu aktual proses di lapangan yang terkait dengan pemanfaatan pattern marker dalam proses cutting untuk kebutuhan manufakturing garment 

## Algoritma Optimasi
Fungsi utama dari modul marker optimization adalah mengupayakan optimasi atas penyebaran laying atas tiap-tiap size dalam PO seoptimal mungkin. Untuk memungkinkan proses ini maka modul membutuhkan keberadaan data-data yang terkait sebagai komponen yang berperan dalam proses penyebaran tersebut. Komponen-komponen tersebut adalah:

### X-Feature Priority
Prioritas atas fitur pada kategori X dan kuantitas order per masing-masing item akan menjadi basis data atas proses pengoptimasian dengan mendefinisikan bentuk operasi terhadap laying rule yang dipilih (free form, even, odd)

### N-Top Most Quantity
Pada dasarnya algoritma yang digunakan sama dengan yang telah dijelaskan pada X-Feature Priority optimisation, hanya saja N-Top Most Quantity tidak menggunakan prioritas pengurutan terhadap feature-X sebagai basis, melainkan sejumlah N atas kuantitas terbesar akan dioptimasi terlebih dulu hingga mencapai titik optimal, kemudian beralih pada sejumlah N-kuantitas terbesar berikutnya.

### N-Bottom Most Quantity
Pada dasarnya algoritma yang digunakan sama dengan yang telah dijelaskan pada X-Feature Priority optimisation, hanya saja seperti halnya N-Top Most Quantity, N-Bottom Most Quantity tidak menggunakan prioritas pengurutan terhadap feature-X sebagai basis, melainkan sejumlah N atas kuantitas terkecil akan dioptimasi terlebih dulu hingga mencapai titik optimal, kemudian beralih pada sejumlah N-kuantitas terkecil berikutnya.