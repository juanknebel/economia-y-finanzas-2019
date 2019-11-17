#xgboost instalo la ultima version de desarrollo de XGBoost que no esta disponible en CRAN R (para histogramas)
cd
sudo rm -rf  xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
cd R-package
sudo R CMD INSTALL .
cd
sudo rm -rf  xgboost


#LightGBM instalo ya que no esta disponible en CRAN R
cd
sudo rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM
cd LightGBM
sudo Rscript ./build_r.R
cd
sudo rm -rf  LightGBM

cd
sudo  Rscript --verbose  ./instalo_paquetes.r
sleep  10

