import React      from 'react';
import styles     from './ToolTip.module.css';


interface Props {
    text: string;
}

const calculateSize = (width: number) => ({
    width: `${width}px`,
    marginLeft: `50%`,
    left: `-${Math.round(width/2)}px`
});


export default (props: Props) =>
    <div className="ToolTip">
        <div className={styles.Bar} style={calculateSize(138)}>
            {props.text}
        </div>
    </div>;
