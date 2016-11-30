<html>
<head>
<script language="VBScript" runat="server">
Set someObj = Nothing
</script>
</head>
<!-- Some html comment -->
<body>
	<div>
		<%
			someStringVar = "text"
			Response.Write(someStringVar)
		%>
	<div>
	<div>
		<span data-attr="<%= "someAttrValue" %>"><%= "inline text" %></span>
	</div>
	<div>
		<span>nested tag</span><br>
		<span>Response.Write("This is not ASP code")</span><br>
		<%
			' Just an asp comment
			someVar = 1
			IF (someVar=1 AND 2 =      3) THEN ' this is comment
				Response.Write("If condition is true!")
			ELSE
		%>
			<span>If condition is false!</span>
		<%
			END IF
		%>
	</div>
</body>
</html>