library(Rcpp)
library(sf)
library(curl)
library(jsonlite)
library(lwgeom)
library(dplyr)
library(purrr)
# cppファイルのコンパイルと読み込み
sourceCpp("pbf2geojsonWrapper.cpp")

tileinfo<-function(x,y,zoom){
  #タイルの左下、右上の　WEBメルカトル座標とm/pixcelを返す
  A<-20037508.342789244
  L<- 2*A/2^zoom
  x0<-L*x-A
  y1<-A-L*y
  x1<-x0+L
  y0<-y1-L
  (list(bbox=c(x0,y0,x1,y1),res=L/256))
}

latlon2tile <- function(lon, lat, zoom) {
  #緯度、経度、ズームレベルからタイル座標を返す
  x = trunc((lon/180 + 1) * 2^zoom/2)
  y = trunc(((-log(tan((45 + lat/2) * pi/180)) + 
                pi) * 2^zoom/(2 * pi)))
  return(c(x,y,zoom))
}

loadMVT<-function(baseurl,ext,x,y,zoom){
  #レイヤ名＆ジオメトリタイプごとのgeojsonをリストで返す
  
  #デバッグ用
  #myurl<-"https://free-0.tilehosting.com/data/v3/14/14597/6328.pbf.pict?key=GiVhgsc1enVLFVtuIdLT"
  #myurl<-"http://map.ecoris.info/mvt-tiles/veg/14/14597/6328.pbf"
  #zoom<-14
  #x<-14597
  #y<-6328
  #tileX<-15666533.3173297
  #tileY<-4559315.86315419
  #tileSpanX<-2445.98490512564
  #tileSpanY<--2445.98490512564

  myurl<-paste0(baseurl, paste(zoom, x, y, sep = "/"), ext)
  info<-tileinfo(x,y,zoom)
  tileX<-info$bbox[1] #タイル左上のWEBメルカトルX座標
  tileY<-info$bbox[4] #タイル左上のWEBメルカトルY座標
  tileSpanX<-info$res*256 #タイルのX方向長さ
  tileSpanY<-info$res*256*-1 #タイルのY方向長さ（負にする）
  #バイナリデータ読み込み
  print(myurl)
  con <- gzcon(curl(myurl))
  mvt <- readBin(con, raw(), n=1e6, endian = "little")
  close(con)
  mvt<-paste(mvt,collapse = "")
  #バイナリデータをjsonに変換（C++のライブラリを呼び出し）
  mvtjson<-decodePBF(zoom,x,y,tileX,tileY,tileSpanX,tileSpanY,mvt)
  Encoding(mvtjson)<-"UTF-8"
  mvtjson<-gsub("_row","__row",mvtjson)
  #jsonをリストに変換して、レイヤ名＆ジオメトリタイプごとにgeojsonを作成
  mvtlist<-fromJSON(mvtjson)
  layer_names<-names(mvtlist)
  geo_types = c("Point","MultiPoint","Polygon","MultiPolygon","LineString","MultiLineString")
  layer_list<-list()
  for(layer_name in layer_names){
    layer<-mvtlist[[layer_name]]
    for(geo_type in geo_types){
      features = layer[[geo_type]]
      if(!is.null(features)&&length(features)!=0){
        empty_geojson<-paste('{"source": "", "tiles": [], "features": [], "crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:EPSG::3857"}}, "type": "FeatureCollection", "scheme": "xyz", "zoom_level":', zoom, ',"layer":"',layer_name,'"}',sep="")
        geojsonlist<-fromJSON(empty_geojson)
        features$id<-NULL
        features$properties$`id`<-seq(1:nrow(features))
        features$properties$`_col`<-NULL
        features$properties$`__row`<-NULL
        features$properties$`_zoom`<-NULL
        geojsonlist$features<-features
        geojson<-toJSON(geojsonlist,auto_unbox = TRUE)
        geo<-read_sf(geojson)
        geo<-st_make_valid(geo)
        layer_type<-paste(layer_name,geo_type,sep="")
        layer_list[[layer_type]]<-geo
      }
    }
  }
  return(layer_list)
}

bind_rows.sf<-function(x){
  #sfクラスのリストをバインドする。列にミスマッチがある場合はNAにする。
  a<-x %>% map(~dplyr::select(.,geometry)) %>% reduce(rbind)
  b<-x %>% map(~st_set_geometry(.,NULL)) %>% reduce(bind_rows)
  geom<-st_sf(bind_cols(a,b))
  return(geom)
}

getMVT<-function(baseurl,ext,lon0,lat0,lon1,lat1,zoom,combine=TRUE){
  #緯度経度の左下～右上の範囲のベクトルタイルを取得
  
  #デバッグ用
  #lon0=140.74963
  #lat0=37.85554
  #lon1=140.75963
  #lat1=37.85554
  #zoom<-14
  #baseurl<-"https://free-0.tilehosting.com/data/v3/"
  #ext<-".pbf.pict?key=GiVhgsc1enVLFVtuIdLT"
  #baseurl<-"http://map.ecoris.info/mvt-tiles/veg/"
  #ext<-".pbf"

  xyz0 <- latlon2tile(lon0,lat0,zoom)
  xyz1 <- latlon2tile(lon1,lat1,zoom)
  features<-list()
  for(y in xyz0[2]:xyz1[2]){
    for(x in xyz0[1]:xyz1[1]){
      layer_list<-loadMVT(baseurl,ext,x,y,zoom)
      tileid<-paste(x,y,sep="-")
      features[[tileid]]<-layer_list
    }
  }
  #tileidとlayer_typeの階層を入れ替える  
  tmp_fet<-unlist(unname(features), recursive=FALSE)
  features <- split(setNames(tmp_fet, rep(names(features), lengths(features))), names(tmp_fet))

    #タイルを1つにする
  if(combine==TRUE){
    features<-features %>% map(bind_rows.sf)
  }
  return(features)
}


#------------------------------------------------------
# example 
#------------------------------------------------------

#植生
baseurl<-"http://map.ecoris.info/mvt-tiles/veg/"
ext<-".pbf"
veg<-getMVT(baseurl,ext,140.74963,37.85554,140.75963,37.85554,14)
veg$vegPolygon["HANREI_N"] %>% plot
merged_veg<-veg$vegPolygon %>% group_by(HANREI_N) %>% summarise_all(first)

#OSM tiled by hfu
baseurl<-"https://hfu.github.io/jp1710_"
ext<-".mvt"
osm<-getMVT(baseurl,ext,139.767052,35.68054,139.797052,35.68054,13)
osm$transportationLineString["class"] %>% plot
osm$buildingPolygon["id"] %>% plot(add=T,col="blue")
