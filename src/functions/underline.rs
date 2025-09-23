//! Underline function implementation for KaTeX Rust
//!
//! This module handles underline symbols in mathematical expressions,
//! migrated from KaTeX's underline.js.

use crate::build_common::{
    VListChild, VListElem, VListKern, VListParam, make_line_span, make_span, make_v_list,
};
use crate::define_function::{FunctionContext, FunctionDefSpec, FunctionPropSpec};
use crate::dom_tree::HtmlDomNode;
use crate::mathml_tree::{MathDomNode, MathNode, MathNodeType, TextNode};
use crate::options::Options;
use crate::parser::parse_node::{NodeType, ParseNode, ParseNodeUnderline};
use crate::types::ParseError;
use crate::{KatexContext, build_html, build_mathml};

/// Registers underline function in the KaTeX context
pub fn define_underline(ctx: &mut KatexContext) {
    ctx.define_function(FunctionDefSpec {
        node_type: Some(NodeType::Underline),
        names: &["\\underline"],
        props: FunctionPropSpec {
            num_args: 1,
            allowed_in_text: true,
            ..Default::default()
        },
        handler: Some(|context: FunctionContext, args, _opt_args| {
            let base = args[0].clone();

            Ok(ParseNode::Underline(ParseNodeUnderline {
                mode: context.parser.mode,
                loc: context.loc(),
                body: Box::new(base),
            }))
        }),
        html_builder: Some(html_builder),
        mathml_builder: Some(mathml_builder),
    });
}

/// HTML builder for underline nodes
fn html_builder(
    node: &ParseNode,
    options: &Options,
    ctx: &KatexContext,
) -> Result<HtmlDomNode, ParseError> {
    let ParseNode::Underline(underline_node) = node else {
        return Err(ParseError::new("Expected Underline node"));
    };

    // Build the inner group
    let inner_group = build_html::build_group(ctx, &underline_node.body, options, None)?;

    // Create the line to go below the body
    let line = make_line_span("underline-line", options, None);

    // Generate the vlist, with the appropriate kerns
    let default_rule_thickness = options.font_metrics().default_rule_thickness;
    let vlist = make_v_list(
        VListParam::FirstBaseline {
            children: vec![
                VListChild::Kern(VListKern {
                    size: default_rule_thickness,
                }),
                VListElem::builder().elem(line.into()).build().into(),
                VListChild::Kern(VListKern {
                    size: 3.0 * default_rule_thickness,
                }),
                VListElem::builder().elem(inner_group).build().into(),
            ],
        },
        options,
    )?;

    Ok(make_span(
        vec!["mord".to_owned(), "underline".to_owned()],
        vec![vlist.into()],
        Some(options),
        None,
    )
    .into())
}

/// MathML builder for underline nodes
fn mathml_builder(
    node: &ParseNode,
    options: &Options,
    ctx: &KatexContext,
) -> Result<MathDomNode, ParseError> {
    let ParseNode::Underline(underline_node) = node else {
        return Err(ParseError::new("Expected Underline node"));
    };

    let mut operator = MathNode::builder()
        .node_type(MathNodeType::Mo)
        .children(vec![MathDomNode::Text(TextNode {
            text: "\u{203e}".to_owned(),
        })])
        .build();

    operator
        .attributes
        .insert("stretchy".to_owned(), "true".to_owned());

    let mut node = MathNode::builder()
        .node_type(MathNodeType::Munder)
        .children(vec![
            build_mathml::build_group(ctx, &underline_node.body, options)?,
            MathDomNode::Math(operator),
        ])
        .build();

    node.attributes
        .insert("accentunder".to_owned(), "true".to_owned());

    Ok(MathDomNode::Math(node))
}
