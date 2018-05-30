.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.itemNaming	<-	list(
    attribute_column_prefix = '_',
    special_column_prefix = '__',
    includes_column_name	=	'includes'
  )
  toset	<-	!(names(op.itemNaming)	%in%	names(op))
  if(any(toset))	options(op.itemNaming[toset])
  invisible()
}
