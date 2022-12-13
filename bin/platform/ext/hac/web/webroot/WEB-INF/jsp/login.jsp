<%@ page session="false" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"  %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<!DOCTYPE html>
<html>

<head>
    <title>hybris administration console | Login</title>
    <meta charset="utf-8"/>
    <sec:csrfMetaTags />
    <sec:authorize access="isAuthenticated()">
		<c:redirect url="/"/>
    </sec:authorize>
    <link rel="stylesheet" href="<c:url value="/static/css/blueprint.css"/>" type="text/css" media="screen, projection">
	<link rel="stylesheet" href="<c:url value="/static/css/plugins/fancy-type/screen.css"/>" type="text/css" media="screen, projection">
	<link rel="stylesheet" href="<c:url value="/static/css/plugins/buttons/screen.css"/>" type="text/css" media="screen, projection">
	<script type="text/javascript" src="<c:url value="/static/js/jquery/jquery-3.5.1.min.js"/>"></script>
	<link rel="stylesheet" href="<c:url value="/static/css/style.css"/>" type="text/css" media="screen, projection">
	<link rel="shortcut icon" href="<c:url value="/static/img/favicon.png"/>"/>
	<link rel="icon" href="<c:url value="/static/img/favicon.png"/>" type="image/x-icon">
	<!-- redirect_detection - do not remove! -->
</head>
<body class="logincontainer">

<form action="<c:url value="/j_spring_security_check"/>" method="POST">
	<div id="logincontrols" class="logincontrols">
		<div id="loginErrors">&nbsp;
			<c:if test="${not empty param.login_error}">
				<c:out value="Login failed: ${SPRING_SECURITY_LAST_EXCEPTION.message}" />
			</c:if>
		</div>
		<ul>
			<li>
				<input type="text" name="j_username" placeholder="Username"/>
			</li>
			<li>
				<input type="password" name="j_password" placeholder="Password" value=""/>
			</li>
			<li>
				<label><input type="checkbox" name="_spring_security_remember_me" class="checkbox" id="_spring_security_remember_me"  /> Remember Login</label>
			</li>
			<li>
				<button type="submit" class="button" autofocus>login</button>
				<sec:csrfInput/>
			</li>
			<c:if test="${not empty hacSsoUrl}">
				<li class="singlesignon_login_cell">
					<a href="${hacSsoUrl}">Login with Single Sign On</a>
				</li>
			</c:if>
		</ul>
	</div>
</form>

</body>
</html>