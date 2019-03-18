import React      from 'react';
import classnames from 'classnames';
import styles     from './Button.module.css';

import ToolTip    from '../ToolTip';


interface Props {
    children: string;
    danger?: boolean;
    icon?: string;
    tooltip?: string;
    onClick?: () => void;
}


const classes = (props: Props) => classnames({
    [styles.Root]: true,
    [styles.Root__danger]: props.danger
})


const Button = (props: Props) =>
    <div onClick={props.onClick} className={classes(props)}>
        {
            props.tooltip && <ToolTip text={props.tooltip} />
        }

        {props.children}

        {
            props.icon && <i className={`icofont-${props.icon}`}/>
        }
    </div>;


export default Button;