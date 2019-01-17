<?php 
// Painel Gerencial - SEI
// Autenticação de login e senha

$token = $_GET['token'];
setcookie("tokenPainel",'',time()-60, "/"); // destroi
	
// Checa se eh POST com os campos corretos
if (!isset($_GET['token'])) {
	header("Location: ./");
	exit;
}

include('db.php');

// Remove o token do banco
token_destroy($token);

// Login e senha inválidos
header("Location: ./");

?>
