import React      from 'react';
import classnames from 'classnames';
import styles     from './IconButton.module.css';

import ToolTip    from '../ToolTip';


interface Props {
    active?:  boolean;
    icon?:    string;
    letter?:  string;
    tooltip?: string;
    onClick?: () => void;
}

const classes = (props: Props) => classnames({
    [styles.Root]: true,
    [styles.Root__active]: props.active
})

const IconButton = (props: Props) =>
    <div onClick={props.onClick} className={classes(props)}>
        { props.tooltip && <ToolTip text={props.tooltip} /> }
        {
            props.icon
                ? <i className={`icofont-${props.icon}`}></i>
                : props.letter
        }
    </div>;


export default IconButton;
