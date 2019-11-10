sudo apt-get update  &&  sudo apt-get --yes  upgrade

#LightGBM instalo ya que no esta disponible en CRAN R
cd
sudo rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM

#hackeo, copiando los .cpp y .h modificados
cp -r  ~/cloud/cloud1/LightGBM_hack/*   ~/LightGBM
cd  ~/LightGBM/helpers
python  ./parameter_generator.py

#continuo instalacion
cd
cd LightGBM
sudo Rscript ./build_r.R

#finalmente borro
cd
sudo rm -rf  LightGBM





#reacomodo y limpio el desorden de las instalaciones
sudo apt-get --yes  autoremove






