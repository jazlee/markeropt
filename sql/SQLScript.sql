/*
  Unit of measurement
*/
create table umcodes(
  umcode         varchar(4) not null,
  umname         varchar(16),
  umdesc         varchar(48),
  umoptx         varchar(8),
  constraint pk_umcodes primary key(umcode)
);

/*
  Unit of measurement conversion
*/
create table umconv(
  basumcode      varchar(4) not null,
  altumcode      varchar(4) not null,
  umcvtfact      numeric(15, 4),
  umcvtmeth      integer default 0,
);

/*
  Feature list
  -
  ftgrp:
    0 - X Feature (umumnya digunakan sebagai size)
    1 - Y Feature (umumnya digunakan sebagai color)
*/
create table feature(
  ftname         varchar(8) not null,
  ftgrp          integer default 0,
  ftdesc         varchar(38)
  constraint pk_colfeature primary key(ftname)
);

/*
  Feature Items
  -
  fitprio - prioritas pengurutan
*/
create table featureitem(
  ftname         varchar(4) not null,
  fitname        varchar(16) not null,
  fitprio        integer default 5,
  fitdesc        varchar(38)
  constraint pk_colitem primary key(ftname, fitname)
);

create index idx_featureitem_1 on featureitem(ftname, fitprio);

/*
  Default Laying rule
*/
create table layrule(
  lrname        varchar(12) not null,
  lrdesc        varchar(48),
  constraint pk_layrule primary key(lrname)
);

/*
  Default laying rule items
*/
create table layruleitems(
    lrname            varchar(12) not null,
    minpcs            integer not null,
    minlyr            integer not null,
    constraint pk_layruleitems primary key(lrname, minpcs),
);

/*
  List of materials/fabric
*/
create table materials(
  mtname        varchar(16) not null,
  mtdesc        varchar(48),
  constraint pk_materials primary key(mtname)
);

/*
  Type of style
*/
create table styletypes(
  sttname        varchar(8) not null,
  sttdesc        varchar(48),
  constraint pk_styletypes primary key(sttname)
);

/*
  List of style
  -
  stitmcr - flag untuk mengindentifikasikan item sudah dicreate
*/
create table styles(
  stname         varchar(12) not null,
  sttname        varchar(8) not null,
  stdesc         varchar(48),
  umcode         varchar(4),
  stitmcr        smallint default 0,
  constraint pk_styles primary key(stname)
);

/*
  Style connect feature
*/
create table stylefeature(
  stname         varchar(12) not null,
  ftname         varchar(8) not null,
  ftgrp          integer default 0,
  ftdesc         varchar(38),
  constraint pk_stylefeature primary key(stname, ftname)
);

/*
  Style per feature items
*/
create table stylefeatureitems(
  stname         varchar(12) not null,
  ftname         varchar(8) not null,
  fitname        varchar(16) not null,
  fitprio        integer default 5,
  fitdesc        varchar(38),
  constraint pk_stylefeatureitems primary key(stname, ftname, fitname)
);

/*
  Material linked for style per feature items
  -
  tabel hanya tersedia/bisa diakses bila
  fitur yang dimasukkan pada tabel stylefeature
  adalah kategori Y (color).

  mtname - material consuming
  lrname - laying rule name
*/
create table stylefeatureitems_material(
  stname         varchar(12) not null,
  ftname         varchar(8) not null,
  fitname        varchar(16) not null,
  mtname         varchar(16) not null,
  mtdesc         varchar(48),
  lrname         varchar(12) not null,
  lrdesc         varchar(48),
  avgyy          numeric(14, 6),
  umcode         varchar(4),
  fballowance    numeric(12,3),
  fbaltype       integer,
  constraint pk_stylefeatureitems_material primary key(stname, ftname, fitname,
mtname)
);

/*
  Laying rule items for style per feature items
*/
create table stylefeatureitems_rule(
  stname         varchar(12) not null,
  ftname         varchar(8) not null,
  fitname        varchar(16) not null,
  mtname         varchar(16) not null,
  minpcs         integer not null,
  minlyr         integer not null,
  constraint pk_stylefeatureitems primary key(stname, ftname, fitname, mtname,
minpcs)
);

/*
  Actual items as on X + Y (Size + Color) feature
*/
create table items(
  itmname         varchar(48) not null,
  itmdesc         varchar(48),
  stname          varchar(12),
  xftname         varchar(8) not null,
  xfitname        varchar(16) not null,
  xfitprio        integer default 5,
  yftname         varchar(8) not null,
  yfitname        varchar(16) not null,
  umcode          varchar(4),
  constraint pk_items primary key(itmname)
);

/*
  Material per items
*/
create table items_material(
  itmname         varchar(48) not null,
  mtname          varchar(16) not null,
  mtdesc          varchar(48),
  lrname          varchar(12) not null,
  lrdesc          varchar(48),
  avgyy           numeric(14, 6),
  umcode          varchar(4),
  fballowance    numeric(12,3),
  fbaltype       integer,
  constraint pk_items_material primary key(itmname, mtname)
);

/*
  Laying rule used on material per items
*/
create table items_material_rule(
  itmname         varchar(48) not null,
  mtname          varchar(16) not null,
  minpcs          integer not null,
  minlyr          integer not null,
  constraint pk_items_material_rule primary key(itmname, mtname, minpcs)
);

/*
  Actual Orders Definition
*/
create table orders(
  ordno           varchar(16) not null,
  orddesc         varchar(48),
  cstcode         varchar(8),
  cstname         varchar(32),
  dsnfile         varchar(48),
  dsncontent      blob(0, 1),
  maxlength       numeric(12,3),
  mrklength       numeric(12,3),
  mrkwidth        numeric(12,3),
  algorithm       varchar(4),
  lroperation     integer,
  lrmethod        integer,
  lrmethodcnt     integer,
  constraint pk_orders primary key(ordno)
);

/*
  Actual item per order definition
*/
create table orderitems(
  ordno           varchar(16) not null,
  itmname         varchar(48) not null,
  ordqty          integer,
  umcode          varchar(4),
  dsnfile         varchar(48),
  dsncontent      blob(0, 1),
  maxlength       numeric(12,3),
  mrklength       numeric(12,3),
  mrkwidth        numeric(12,3),
  algorithm       varchar(4),
  lroperation     integer,
  lrmethod        integer,
  lrmethodcnt     integer,
  constraint pk_orders primary key(ordno, itmname)
);

/*
  Actual material used for item per order definition
*/
create table orderitems_material(
  ordno           varchar(16) not null,
  itmname         varchar(48) not null,
  mtname          varchar(16) not null,
  lrname          varchar(12) not null,
  avgyy           numeric(14, 6),
  umcode          varchar(4),
  fballowance     numeric(12,3),
  fbaltype        integer,
  dsnfile         varchar(48),
  dsncontent      blob(0, 1),
  maxlength       numeric(12,3),
  mrklength       numeric(12,3),
  mrkwidth        numeric(12,3),
  algorithm       varchar(4),
  lroperation     integer,
  lrmethod        integer,
  lrmethodcnt     integer,
  constraint pk_items_material primary key(ordno, itmname, mtname)
);



Tambahan
=-=-=-=-=-=-=-
  * maksimum layer, digunakan pada saat bikin cutting instruction


 Algorithm
===========
  * X-Feature Priority
  * X-Feature Reversal Priority
  * N-Top Least Quantity
  * N-Top Most Quantity
  * N-Bottom Least Quantity
  * N-Bottom Most Quantity

quantity
  1,2,3,4,5

NTop Least       NTop Most
3  start here    3
4                4
5                5   start here

NBottom Least    NBottom Most
3  start here    3
2                2
1                1   start here