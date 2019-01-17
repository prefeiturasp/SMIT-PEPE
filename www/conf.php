<?php 
// Painel Gerencial - SEI
//
// Configurações necessárias
//

// Endereço da instancia rodando o Shiny-Server
define('SHINY_URL', 'http://pepe.prefeitura.sp.gov.br:16080/'); 

// Banco de Dados do PEPE
define('DB_ADDRESS', 'localhost');
define('DB_USER', 'user');
define('DB_PASS', 'password');
define('DB_DATABASE', 'database');

// Endereço do LDAP se a autenticação for por esse método
define('AUTH_METHOD', 'DB'); //opcoes: LDAP ou DB 
define('LDAP_ADD', '10.10.10.10'); // Endereço do LDAP para autenticação
define('LDAP_SUFIXO', ''); // @redeinterna.rede (Sufixo, se houver)

if(count(get_included_files()) ==1) {
	header("Location: ./");
	exit("Direct access not permitted.");
}

// Conexão BD de permissões
$link = mysqli_connect(DB_ADDRESS, DB_USER, DB_PASS, DB_DATABASE);

if (!$link) {
    echo "Error: Unable to connect to Database." . PHP_EOL;
    echo "Debugging errno: " . mysqli_connect_errno() . PHP_EOL;
    echo "Debugging error: " . mysqli_connect_error() . PHP_EOL;
    exit;
}

// Funcao para login. 
// Esta configurada para usar o LDAP como autenticador
// Se necessário, alterar
function login($login, $senha) {
	global $link;
	$metodo = AUTH_METHOD;
	
	
	// Se LDAP e DB
	if ($metodo == 'LDAP' && $result = mysqli_query($link, "SELECT id FROM user WHERE login='".$login."'")) {
		if (!login_ldap($login, $senha)) {
			if (!$result = mysqli_query($link, "SELECT id FROM user WHERE login='".$login."' AND password='".$senha."'")) return false;
		}
		if (mysqli_num_rows($result)>0) return mysqli_fetch_row($result)[0];
		mysqli_free_result($result);
	} 
	
	// Se DB 
	else if ($metodo == 'DB') {
		$result = mysqli_query($link, "SELECT id FROM user WHERE login='".$login."' AND password='".$senha."'");
		if (mysqli_num_rows($result)>0) return mysqli_fetch_row($result)[0];
		mysqli_free_result($result);
	}
	
	return false;
}

function login_ldap($login, $senha) {
	// Teste LDAP prefeitura (Login + sufixo, se necessário)
	$ldaprdn  = $login.LDAP_SUFIXO;
	
	// conectar ao LDAP
	$ldapconn = ldap_connect(LDAP_ADD) or die("Não foi possível se conectar ao servidor LDAP.");

	// Tentar autenticar
	if ($ldapconn) {
		$ldapbind = ldap_bind($ldapconn, $ldaprdn, $senha);

		// verifica se autenticou ou não.
		if ($ldapbind) return true;
		else return false;
	}
	return false;
}

function token($id_user) {
	global $link;
	// 1. Deleta os tokens expirados, criados a mais de 24h
	mysqli_query($link, "DELETE FROM auth WHERE id_user='".$id_user."' AND created_at<DATE_SUB(CURDATE(), INTERVAL 1 DAY)");
	
	// 2. Cria um novo token
	$new_token = hash('sha256', $id_user.':'.rand());
	$insert_token = mysqli_query($link, "INSERT INTO auth (token,id_user,created_at) VALUES ('".$new_token."', ".$id_user.", NOW());");
	if ($insert_token) return $new_token;
	else return null;
}

function token_destroy($token) {
	global $link;
	mysqli_query($link, "DELETE FROM auth WHERE token='".$token."'");
}


?>
