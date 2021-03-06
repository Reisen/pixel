import React      from 'react';
import classnames from 'classnames';
import styles     from './IconButton.module.css';

import ToolTip    from '../ToolTip';


interface Props {
    active?:  boolean;
    icon?:    string;
    letter?:  string;
    onClick?: () => void;
    small?:   boolean;
    tooltip?: string;
}

const classes = (props: Props) => classnames({
    [styles.Root]: true,
    [styles.Root__active]: props.active,
    [styles.Root__small]: props.small
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
