#!/usr/bin/env bash

BASE=$(cd $(dirname $0) && pwd)

mix_aux()
{
  $BASE/dist/build/mix-kenall-geocode/mix-kenall-geocode \
          -g <(gunzip --stdout $BASE/data/geo-location-data-of-japan/data/${1}.csv.gz | gawk "${2}<=NR && NR<${3}")
}

mix() 
{
  local TMP1=$(mktemp) 
  local TMP2=$(mktemp) 
  local TMP3=$(mktemp) 
  local TMP4=$(mktemp) 

  grep $1 $BASE/data/ken_all/data/KEN_ALL.CSV | ./dist/build/change-fmt-kenall/change-fmt-kenall > $TMP1
  mix_aux $2 0 30000 < $TMP1 > $TMP2
  mix_aux $2 30000 60000 < $TMP2 > $TMP3
  mix_aux $2 60000 90000 < $TMP3 > $TMP4
  mix_aux $2 90000 150000 < $TMP4
}

mix 北海道 01_hokkaido
mix 青森県 02_aomori
mix 岩手県 03_iwate
mix 宮城県 04_miyagi
mix 秋田県 05_akita
mix 山形県 06_yamagata
mix 福島県 07_fukushima
mix 茨城県 08_ibaraki
mix 栃木県 09_tochigi
mix 群馬県 10_gunma
mix 埼玉県 11_saitama
mix 千葉県 12_chiba
mix 東京都 13_tokyo
mix 神奈川県 14_kanagawa
mix 新潟県 15_niigata
mix 富山県 16_toyama
mix 石川県 17_ishikawa
mix 福井県 18_fukui
mix 山梨県 19_yamanashi
mix 長野県 20_nagano
mix 岐阜県 21_gifu
mix 静岡県 22_shizuoka
mix 愛知県 23_aichi
mix 三重県 24_mie
mix 滋賀県 25_shiga
mix 京都府 26_kyoto
mix 大阪府 27_osaka
mix 兵庫県 28_hyogo
mix 奈良県 29_nara
mix 和歌山県 30_wakayama
mix 鳥取県 31_tottori
mix 島根県 32_shimane
mix 岡山県 33_okayama
mix 広島県 34_hiroshima
mix 山口県 35_yamaguchi
mix 徳島県 36_tokushima
mix 香川県 37_kagawa
mix 愛媛県 38_ehime
mix 高知県 39_kochi
mix 福岡県 40_fukuoka
mix 佐賀県 41_saga
mix 長崎県 42_nagasaki
mix 熊本県 43_kumamoto
mix 大分県 44_oita
mix 宮崎県 45_miyazaki
mix 鹿児島県 46_kagoshima
mix 沖縄県 47_okinawa

