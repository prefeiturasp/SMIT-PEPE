<?php
if (isset($_COOKIE['tokenPainel']) && !isset($_GET['error']) && !isset($_GET['exp'])) {
	$link = 'http://pepe.prefeitura.sp.gov.br:16080/'; // Link do painel R (/ no final)
	header("Location: ".$link.'?token='.$_COOKIE['tokenPainel']);
	exit;
} else {
	setcookie("tokenPainel",'',time()-60, "/"); // destroi
}
?>

<!DOCTYPE html>
<html >
<head>
  <meta charset="UTF-8">
  <title>Painel Gerencial - SEI</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css">
  <link rel='stylesheet prefetch' href='https://fonts.googleapis.com/css?family=Roboto:400,100,300,500,700,900|RobotoDraft:400,100,300,500,700,900'>
  <link rel='stylesheet prefetch' href='https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css'>
  <link rel="stylesheet" href="css/style.css">  
</head>

<body> 

<div class="painel-title">
  <img src="img/pepe.png" style="width: 420px; margin-bottom: 10px;"/>
  <h1>Painel Gerencial</h1>
</div>

<?php 
if (isset($_GET['error'])) echo '<div class="error">Login e/ou senha incorreto(s).</div>';
else if (isset($_GET['exp'])) echo '<div class="warning">Refaça o login, por favor.</div>';
?>

<div class="container">
  <div class="card"></div>
  <div class="card">
    <h1 class="title">Entrar</h1>
	<form action="processa_login.php" method="POST">
      <div class="input-container">
        <input type="text" id="login" name="login" autocomplete="off" required="required"/>
        <label for="login">Usuário</label>
        <div class="bar"></div>
      </div>
      <div class="input-container">
        <input type="password" id="senha" name="senha" required="required"/>
        <label for="senha">Senha</label>
        <div class="bar"></div>
      </div>
	  <div class="button-container">
        <button><span>Entrar</span></button>
		<div class="cb-container">
		<input id="remember" name="remember" type="checkbox">
		<label_cb>Manter-se conectado</label_cb>
		</div>
      </div>
    </form>
  </div>
</div>

<div class="footer">
  <img src="img/processo_eletronico_vert.png" style="width: 100px; margin-bottom: 10px;"/>
</div>

<a id="link_sei" href="//sei.prefeitura.sp.gov.br/" target="_blank" title="Acesso ao SEI"><i class="fa fa-link"></i></a>
<script src='//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js'></script>
<script  src="js/index.js"></script>

</body>
</html>
