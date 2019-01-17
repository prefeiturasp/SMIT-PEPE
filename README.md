# PEPE

PEPE é um painel de gerenciamento dos processos eletrônicos, criado pela Prefeitura de São Paulo para uso com o conjunto de dados do sistema SEI (Sistema Eletrônico de Informações), desenvolvido pelo TRF4.

## Resumo de funcionamento
O PEPE funciona com o Shiny-Server Open Source e uma camada de autenticação anterior através de uma página PHP.
No código exemplo, utilizamos a integração com o AD para fazer a autenticação com os usuários e um banco de dados auxiliar para ditar as permissões do usuário no caso de uma instalação multi-órgãos.

- Pasta www: Source PHP da página de autenticação, modelo do banco de dados de acesso e 
- Pasta R: Sources em R para geração dos gráficos e visualização através do shiny-server

## Requisitos
* R 3.0.0 ou superior
* MariaDB/MySQL para gerenciamento de acessos
* Apache+PHP (Ou nginx+PHP)
* Bibliotecas do R: 
* Shiny-Server (versão Open Source)

## Configuração Recomendada
* 4 vCPUs 
* 4GB+ RAM

## Instruções (Pós instalação das páginas de login e administração)

### Instalação do R
(Instruções detalhadas em https://www.r-project.org/)
    wget https://cran-r.c3sl.ufpr.br/src/base/R-3/R-3.5.2.tar.gz
	tar -xf R-3.5.2.tar.gz
	cd R-3.5.2
	./configure 
####Se não tiver as bibliotecas necessárias para a compilação, o script irá alertar. Instale-as para prosseguir - No Ubuntu Server, tivemos que usar :
apt-get install gfortran libreadline-dev xorg-dev liblzma-dev  libblas-dev libbz2-dev libcurl4-openssl-dev####
	make
	make install
	apt-get install unixODBC*

#### Packages do R necessários
Com root ou sudo, entrar no R (comando R no bash) e instalar os seguintes pacotes:
	- install.packages('RODBC')
	- install.packages('ggplot2')
	- install.packages('dplyr')
	- install.packages('shiny')
	- install.packages('shinydashboard')
	- install.packages('DT')
	- install.packages('reshape')
	- install.packages('scales')
	- install.packages('shinyjs')
	- install.packages('V8')
	- install.packages('htmlwidgets')
	- install.packages('js')
	- install.packages('plotly')
	- install.packages('shinyBS')
	- install.packages('openxlsx')

### Instalação do Shiny Server Open Source
(Instruções retiradas de https://www.rstudio.com/products/shiny/download-server/)
	$ sudo apt-get install gdebi-core
	$ wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb
	$ sudo gdebi shiny-server-1.5.9.923-amd64.deb
	
### Configuração
	O arquivo de configuração do shiny está localizado em /etc/shiny-server/shiny-server.conf
	Por padrão, o Shiny-Server configura o servidor na porta 3838. 
	Recomendamos alterá-la (e deixar a porta 80 livre para o Apache).
	Exemplo de shiny-server.conf:
		
	run_as shiny;
	server {
		listen 38080;
		location / {
			site_dir /srv/shiny-server;
			log_dir /var/log/shiny-server;
			access_log /var/log/shiny-server/access.log tiny;
			directory_index off;
			app_init_timeout 9800;
			app_idle_timeout 259200;
		}
	}

	Note que: 
	1 - Os arquivos .R deverão ficar localizados na pasta apontada em site_dir;
	2 - O app_init_timeout pode ser elevado. Recomendamos que seja feito um cron para reiniciar o shiny diariamente, durante a madrugada, para que ele atualize as bases de dados.
	
	Ao acessar, o endereço http://x.x.x.x:porta, você deverá ver uma mensagem "Welcome to Shiny Server"!
	
## Banco de Dados
	O modelo do BD está disponível no arquivo pepe.sql;
	Deve ser criado um banco com o esquema do arquivo e um usuário para leitura e escrita, que serão usados na configuração a seguir.
	Obs.: Os arquivos estão configurados para usarem o MySQL/mariaDB, mas podem ser adaptados para algum outro banco.
	
## Funcionamento
	Passo 1 - Editar o www/conf.php
	Variáveis
	- SHINY_URL: URL em que o shiny-server ficará acessível, seguido da porta configurada no parâmetro listen;
	- DB_ADDRESS - Endereço do Banco de Dados
	- DB_USER - Usuário do banco de dados
	- DB_PASS - Senha do usuário do banco de dados
	- DB_DATABASE - Nome do Banco de Dados
	
	- AUTH_METHOD: 	DB (Se o usuário for cadastrado com login e senha) ou
					LDAP (Se for utilizado um servidor de autenticação).
					
	Para ambos métodos acima, as permissões do usuário deverão ser cadastradas no banco de dados acima configurado. Se for usar o LDAP, também configurar o LDAP_ADD (endereço do LDAP) e o LDAP_SUFIXO, se houver.
	
	Passo 2 - Após edição da configuração, os arquivos da pasta www devem ser copiados para o servidor do Apache, no endereço em que ficará localizada a página de login.
	
	Passo 3 - Editar o arquivo R/global.r; Configurar o endereço do banco de dados do SEI, com o servidor, database, usuário e senha (somente leitura)
	
	Passo 4 - Editar o arquivo R/server.r; Configurar o endereço do banco de dados do PEPE, com o servidor e database criados;
	
	Passo 5 - Os arquivos da pasta R devem ser copiados para /srv/shiny-server (ou o diretório configurado no parâmetro site_dir);
	
	Passo 6 - Testar o login padrão (Login: pepe; Senha: pepe).
	
Pronto!
Lembrando que o sistema foi desenvolvido com base nas necessidades da prefeitura de São Paulo. No entanto, é permitida qualquer alteração no código para adequar às necessidades próprias.

	

