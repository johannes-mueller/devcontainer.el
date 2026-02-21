# texi2any init file: suppress inline CSS while keeping --css-ref links.
# Overrides the default format_css_lines handler which emits both an inline
# <style> block and <link> tags.  This replacement emits only the <link> tags.
texinfo_register_formatting_function('format_css_lines', sub {
  my $self = shift;
  my $filename = shift;

  return '' if ($self->get_conf('NO_CSS'));

  my $css_refs = $self->get_conf('CSS_REFS');
  return '' if (!defined($css_refs) or !@$css_refs);

  my $css_text = '';
  foreach my $ref (@$css_refs) {
    $css_text .= $self->close_html_lone_element(
         '<link rel="stylesheet" type="text/css" href="'.
                $self->url_protect_url_text($ref).'"')."\n";
  }
  return $css_text;
});

1;
