<?php 
// Painel Gerencial - SEI
// Autenticação de login e senha
include('conf.php');

function redirect_r($token) {
	header("Location: ".SHINY_URL.'?token='.$token);
}

$postUser = $_POST['login'];
$postPass = md5($_POST['senha']);

// Checa o metodo POST com os campos corretos
if (!isset($_POST['login']) && !isset($_POST['senha'])) {
	header("Location: ./");
	exit;
}

// Login efetuado com sucesso
if (login($postUser,$postPass)) {
	$id_user = login($postUser,$postPass);
	$token = token($id_user);
	
	// Checa se foi marcado a opcao para guardar o login
	if (isset($_POST['remember']))
		setcookie("tokenPainel",$token,time()+ 60*60*24, "/"); // valido por 24h
	else 
		setcookie("tokenPainel",'',time()-60, "/"); // destroi

	// Redireciona para o R (Shiny-Server)
	redirect_r($token);
}

// Login e senha inválidos
else {
	setcookie("tokenPainel",'',time()-60, "/"); // destroi
	header("Location: ./?error");
}

?>
